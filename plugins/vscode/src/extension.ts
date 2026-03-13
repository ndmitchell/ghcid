'use strict'
import * as vscode from 'vscode'
import * as path from 'path'
import * as fs from 'fs'
import * as os from 'os'
import * as crypto from 'crypto'
import * as util from 'util'
import {
  concatMap,
  distinctUntilChanged,
  fromEventPattern,
  map,
  merge,
  Observable,
  pairwise,
  startWith,
  Subscription,
  tap,
} from 'rxjs'
import { Resource, startGhcidMultiClient } from './util'

const outputChannel = vscode.window.createOutputChannel('ghcid')

const isTest = process.env.VSCODE_TEST === 'true'

const log = (...args: any[]) => {
  const line = util.format(...args)
  outputChannel.appendLine(util.stripVTControlCharacters(line))
  if (isTest) console.log(`[ext] ${line}`)
}

// Regex matching Haskell identifiers including qualified names (e.g. Data.Map.lookup)
// and backtick-quoted infix usage (e.g. `elem`)
const HASKELL_IDENT =
  /`(?:[A-Z][a-zA-Z0-9_']*\.)*[a-zA-Z_][a-zA-Z0-9_']*`|(?:[A-Z][a-zA-Z0-9_']*\.)*[a-zA-Z_][a-zA-Z0-9_']*/

export const parseGhcidOutput = (dir: string, s: string): [vscode.Uri, vscode.Diagnostic][] => {
  const allLines = s.replace(/\r/g, '').split('\n')

  if (allLines.length === 0) return []
  if (allLines[0].startsWith('All good')) return []
  if (allLines[0].startsWith('Ghcid has stopped.')) return []

  const isHeader = (line: string) => /: (error|warning):/.test(line)

  // Group lines into messages, each starting with a location header
  const messages: string[][] = []
  for (const line of allLines) {
    if (isHeader(line)) {
      messages.push([line])
    } else if (messages.length > 0) {
      messages[messages.length - 1].push(line)
    }
  }

  return messages.flatMap(lines => {
    const header = lines[0]
    let m: RegExpMatchArray
    let file: string
    let range: vscode.Range
    let severityStr: string | undefined
    let firstBodyLine = ''

    // file:(line,col)-(line,col)
    if ((m = header.match(/^(.*):\((\d+),(\d+)\)-\((\d+),(\d+)\): (error|warning):(.*)$/))) {
      file = m[1]
      range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[4] - 1, +m[5])
      severityStr = m[6]
      firstBodyLine = m[7].trimStart()
      // file:line:col-col
    } else if ((m = header.match(/^(.*):(\d+):(\d+)-(\d+): (error|warning):(.*)$/))) {
      file = m[1]
      range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[2] - 1, +m[4])
      severityStr = m[5]
      firstBodyLine = m[6].trimStart()
      // file:line:col
    } else if ((m = header.match(/^(.*):(\d+):(\d+): (error|warning):(.*)$/))) {
      file = m[1]
      range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[2] - 1, +m[3])
      severityStr = m[4]
      firstBodyLine = m[5].trimStart()

      // Infer a better range from ^^^ caret annotations if present
      for (let i = 1; i < lines.length; i++) {
        const gutter = lines[i].match(/^\d+ \| /)
        if (gutter && i + 1 < lines.length) {
          const carets = lines[i + 1].match(/\^+/)
          if (carets) {
            const start = carets.index - gutter[0].length
            const end = start + carets[0].length
            range = new vscode.Range(+m[2] - 1, start, +m[2] - 1, end)
            break
          }
        }
      }
    } else {
      return []
    }

    // Determine severity
    let severity = vscode.DiagnosticSeverity.Error
    if (severityStr === 'warning') severity = vscode.DiagnosticSeverity.Warning

    // Build message body
    const bodyLines: string[] = []
    if (firstBodyLine !== '') bodyLines.push(firstBodyLine)
    for (let i = 1; i < lines.length; i++) {
      const line = lines[i]
      if (/^\s+\|$/.test(line)) break // code snipped
      if (/In the/.test(line)) break // expression context
      if (/In a stmt/.test(line)) break // statement context
      bodyLines.push(line)
    }

    // Dedent
    const nonEmpty = bodyLines.filter(l => l !== '')
    if (nonEmpty.length > 0) {
      const indent = Math.min(...nonEmpty.map(l => l.match(/^\s*/)[0].length))
      for (let i = 0; i < bodyLines.length; i++) bodyLines[i] = bodyLines[i].slice(indent)
    }

    file = m[1].replace(/\\/g, '/') // normalize Windows paths
    const uri = vscode.Uri.file(path.isAbsolute(file) ? file : path.join(dir, file))
    return [[uri, new vscode.Diagnostic(range, bodyLines.join('\n'), severity)]]
  })
}

const groupDiagnostic = (xs: [vscode.Uri, vscode.Diagnostic[]][]): [vscode.Uri, vscode.Diagnostic[]][] => {
  let seen = new Map<string, [number, vscode.Uri, vscode.Diagnostic[]]>()
  for (var i = 0; i < xs.length; i++) {
    let key = xs[i][0].path
    if (seen.has(key)) {
      let v = seen.get(key)
      v[2] = v[2].concat(xs[i][1])
    } else seen.set(key, [i, xs[i][0], xs[i][1]])
  }
  return Array.from(seen.values())
    .sort((a, b) => a[0] - b[0])
    .map(x => [x[1], x[2]])
}

const autoWatch = async (stack: AsyncDisposableStack) => {
  const watcher = stack.use(vsCodeDisposableToDisposable(vscode.workspace.createFileSystemWatcher('**/ghcid.txt')))
  const uri2diags = stack.adopt(new Map<string, vscode.DiagnosticCollection>(), m =>
    Array.from(m.values()).forEach(diag => diag.dispose())
  )

  const onUpdate = (uri: vscode.Uri) => {
    const diags = uri2diags.get(uri.fsPath) || vscode.languages.createDiagnosticCollection('ghcid')
    uri2diags.set(uri.fsPath, diags)
    diags.clear()
    diags.set(
      groupDiagnostic(
        parseGhcidOutput(path.dirname(uri.fsPath), fs.readFileSync(uri.fsPath, 'utf8')).map(x => [x[0], [x[1]]])
      )
    )
  }

  const files = await vscode.workspace.findFiles('**/ghcid.txt')
  files.forEach(onUpdate)
  stack.use(vsCodeDisposableToDisposable(watcher.onDidCreate(onUpdate)))
  stack.use(vsCodeDisposableToDisposable(watcher.onDidChange(onUpdate)))
  stack.use(
    vsCodeDisposableToDisposable(
      watcher.onDidDelete(uri => {
        uri2diags.get(uri.fsPath)?.dispose()
        uri2diags.delete(uri.fsPath)
      })
    )
  )
}

const getWordRange = (document: vscode.TextDocument, position: vscode.Position): vscode.Range | undefined => {
  return document.getWordRangeAtPosition(position, HASKELL_IDENT)
}

const buildCommand = (prefix: string, document: vscode.TextDocument, range: vscode.Range): string => {
  const file = document.uri.fsPath
  const startLine = range.start.line + 1
  const startCol = range.start.character + 1
  const endLine = range.end.line + 1
  const endCol = range.end.character
  return `${prefix} ${file} ${startLine} ${startCol} ${endLine} ${endCol}`
}

const toLocationUri = (workspaceRoot: string, file: string): vscode.Uri => {
  return path.isAbsolute(file) || !workspaceRoot
    ? vscode.Uri.file(file)
    : vscode.Uri.file(path.join(workspaceRoot, file))
}

const parseLocation = (workspaceRoot: string, line: string): vscode.Location | undefined => {
  const trimmed = line.trim()
  let m = trimmed.match(/^(.*):\((\d+),(\d+)\)-\((\d+),(\d+)\)$/)
  if (m) {
    const uri = toLocationUri(workspaceRoot, m[1])
    const range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[4] - 1, +m[5])
    return new vscode.Location(uri, range)
  }

  m = trimmed.match(/^(.*):(\d+):(\d+)-(\d+)$/)
  if (m) {
    const uri = toLocationUri(workspaceRoot, m[1])
    const range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[2] - 1, +m[4])
    return new vscode.Location(uri, range)
  }

  m = trimmed.match(/^(.*):(\d+):(\d+)$/)
  if (m) {
    const uri = toLocationUri(workspaceRoot, m[1])
    const range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[2] - 1, +m[3])
    return new vscode.Location(uri, range)
  }

  return undefined
}

const HASKELL_DOCUMENT_SELECTOR: vscode.DocumentSelector = [
  { scheme: 'file', language: 'haskell' },
  { scheme: 'file', pattern: '**/*.hs' },
  { scheme: 'file', pattern: '**/*.lhs' },
  { scheme: 'file', pattern: '**/*.hs-boot' },
]

const newContextStack = (context: vscode.ExtensionContext) => {
  const stack = new AsyncDisposableStack()
  context.subscriptions.push({ dispose: () => stack.disposeAsync() })
  return stack
}

const subscriptionToDisposable = (subscription: Subscription): Disposable => ({
  [Symbol.dispose]: () => subscription.unsubscribe(),
})

const vsCodeDisposableToDisposable = <T extends vscode.Disposable>(disposable: T): T & Disposable =>
  Object.assign(disposable, {
    [Symbol.dispose]() {
      disposable.dispose()
    },
  })

const fromEventPatternDisposable = <T>(e: vscode.Event<T>): Observable<T> =>
  fromEventPattern<T>(
    handler => e(handler),
    (_, disposable: vscode.Disposable) => disposable.dispose()
  )

let rootStack: AsyncDisposableStack | undefined = undefined

const connectToServer = async (stack: AsyncDisposableStack): Promise<void> => {
  const socketDiagnostics = stack.adopt(new Map<string, vscode.DiagnosticCollection>(), diags => {
    for (const collection of socketDiagnostics.values()) collection.dispose()
    socketDiagnostics.clear()
  })

  const controller = stack.adopt(new AbortController(), c => c.abort())
  const ghcidClient = stack.use(
    await startGhcidMultiClient({
      onDiagnostics: (workspaceRoot, diagnostics) => {
        const diagnosticsForRoot = groupDiagnostic(
          parseGhcidOutput(workspaceRoot, diagnostics).map(x => [x[0], [x[1]]])
        )
        const collection =
          socketDiagnostics.get(workspaceRoot) ??
          vscode.languages.createDiagnosticCollection(`ghcid-socket:${workspaceRoot}`)
        socketDiagnostics.set(workspaceRoot, collection)
        collection.clear()
        collection.set(diagnosticsForRoot)
      },
      log,
    })(controller.signal)
  )

  stack.use(
    subscriptionToDisposable(
      fromEventPatternDisposable(vscode.workspace.onDidChangeWorkspaceFolders)
        .pipe(
          startWith({ added: vscode.workspace.workspaceFolders, removed: [] }),
          concatMap(async v => {
            for (const root of v.removed) await ghcidClient.removeDir(root.uri.fsPath)
            for (const root of v.added) await ghcidClient.addDir(root.uri.fsPath)
            for (const root of v.removed) {
              socketDiagnostics.get(root)?.dispose()
              socketDiagnostics.delete(root)
            }
          })
        )
        .subscribe({
          error: err => {
            log(`socket client workspace sync failed: ${err}`)
          },
        })
    )
  )

  stack.use(
    vsCodeDisposableToDisposable(
      vscode.languages.registerHoverProvider(HASKELL_DOCUMENT_SELECTOR, {
        provideHover: async (document, position) => {
          const range = getWordRange(document, position)
          if (!range || !ghcidClient) return undefined
          const word = document.getText(range)
          try {
            const request = buildCommand(':type-at', document, range)
            const { output } = await ghcidClient.request(document.uri.fsPath, request)
            if (!output.trim()) return undefined
            if (output.includes('no location info')) return undefined
            if (output.includes(`Couldn't guess that module name`)) {
              log(
                `socket client: hover failed: ghcid couldn't guess module name, make sure you pass the right --target to ghcid. Request: ${JSON.stringify(request)}`
              )
              return undefined
            }
            const ident = word.replace(/^`|`$/g, '')
            const markdown = new vscode.MarkdownString()
            markdown.appendCodeblock(ident + output, 'haskell')
            return new vscode.Hover(markdown, range)
          } catch (err) {
            log(`socket client: hover failed: ${err instanceof Error ? err.message : String(err)}`)
            return undefined
          }
        },
      })
    )
  )

  stack.use(
    vsCodeDisposableToDisposable(
      vscode.languages.registerDefinitionProvider(HASKELL_DOCUMENT_SELECTOR, {
        provideDefinition: async (document, position) => {
          const range = getWordRange(document, position)
          if (!range || !ghcidClient) return undefined
          try {
            const result = await ghcidClient.request(document.uri.fsPath, buildCommand(':loc-at', document, range))
            return parseLocation(result.workspaceRoot, result.output)
          } catch (err) {
            log(`socket client: definition failed: ${err instanceof Error ? err.message : String(err)}`)
            return undefined
          }
        },
      })
    )
  )

  stack.use(
    vsCodeDisposableToDisposable(
      vscode.languages.registerReferenceProvider(HASKELL_DOCUMENT_SELECTOR, {
        provideReferences: async (document, position) => {
          const range = getWordRange(document, position)
          if (!range || !ghcidClient) return undefined
          try {
            const result = await ghcidClient.request(document.uri.fsPath, buildCommand(':uses', document, range))
            const locations = result.output
              .split(/\r?\n/)
              .map(line => parseLocation(result.workspaceRoot, line))
              .filter((location): location is vscode.Location => location !== undefined)
            return locations
          } catch (err) {
            log(`socket client: references failed: ${err instanceof Error ? err.message : String(err)}`)
            return undefined
          }
        },
      })
    )
  )
}

const addStartGhcidCommand = async (stack: AsyncDisposableStack): Promise<void> => {
  const terminalToDisposable = new Map<vscode.Terminal, Disposable>()

  stack.use(
    vsCodeDisposableToDisposable(
      vscode.window.onDidCloseTerminal(t => {
        terminalToDisposable.get(t)?.[Symbol.dispose]()
      })
    )
  )

  stack.use(
    vsCodeDisposableToDisposable(
      vscode.commands.registerCommand('extension.startGhcid', async () => {
        const roots = vscode.workspace.workspaceFolders
        const root = await (async () => {
          if (roots.length === 0) {
            return undefined
          } else if (roots.length === 1) {
            return roots[0]
          } else {
            return await vscode.window.showWorkspaceFolderPick()
          }
        })()

        if (!root) {
          vscode.window.showWarningMessage('You must open a workspace first.')
          return
        }

        const hash = crypto.createHash('sha256').update(root.uri.fsPath).digest('hex').slice(0, 20)
        const file = stack.adopt(path.join(os.tmpdir(), `ghcid-${hash}.txt`), f => {
          try {
            fs.unlinkSync(f)
          } catch {}
        })
        fs.writeFileSync(file, '')

        const ghcidCommand: string = vscode.workspace.getConfiguration('ghcid').get('command')
        const extraArgs: string[] = [`--outputfile=${file}`]

        const opts: vscode.TerminalOptions = {
          name: 'ghcid',
          shellPath: os.type().startsWith('Windows') ? 'cmd.exe' : ghcidCommand,
          shellArgs: [...(os.type().startsWith('Windows') ? ['/k', ghcidCommand] : []), ...extraArgs],
          cwd: root.uri.fsPath,
        }

        const terminal = stack.use(vsCodeDisposableToDisposable(vscode.window.createTerminal(opts)))
        terminal.show()

        terminalToDisposable.set(
          terminal,
          (() => {
            const d = vscode.languages.createDiagnosticCollection('ghcid')
            const go = () => {
              d.clear()
              d.set(
                groupDiagnostic(
                  parseGhcidOutput(root.uri.fsPath, fs.readFileSync(file, 'utf8')).map(x => [x[0], [x[1]]])
                )
              )
            }
            const watcher = fs.watch(file, go)
            go()
            return {
              [Symbol.dispose]: () => {
                try {
                  fs.unlinkSync(file)
                } catch {}
                watcher.close()
                d.clear()
              },
            }
          })()
        )
      })
    )
  )
}

export const activate = async (context: vscode.ExtensionContext): Promise<void> => {
  rootStack = newContextStack(context)
  await connectToServer(rootStack)
  await addStartGhcidCommand(rootStack)
  await autoWatch(rootStack)
}

export const deactivate = () => {
  rootStack?.disposeAsync()
}
