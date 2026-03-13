'use strict'
import * as vscode from 'vscode'
import * as path from 'path'
import * as fs from 'fs'
import * as os from 'os'
import * as crypto from 'crypto'
import * as util from 'util'
import { concatMap, distinctUntilChanged, fromEventPattern, map, pairwise, startWith, Subscription, tap } from 'rxjs'
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

const watchOutput = (root: string, file: string): fs.FSWatcher => {
  let d = vscode.languages.createDiagnosticCollection('ghcid')
  let go = () => {
    d.clear()
    d.set(groupDiagnostic(parseGhcidOutput(root, fs.readFileSync(file, 'utf8')).map(x => [x[0], [x[1]]])))
  }
  let watcher = fs.watch(file, go)
  go()
  return watcher
}

const autoWatch = async (context: vscode.ExtensionContext) => {
  // TODO support multiple roots
  const watcher = vscode.workspace.createFileSystemWatcher('**/ghcid.txt')
  context.subscriptions.push(watcher)
  const uri2diags = new Map<string, vscode.DiagnosticCollection>()
  context.subscriptions.push({ dispose: () => Array.from(uri2diags.values()).forEach(diag => diag.dispose()) })

  const onUpdate = (uri: vscode.Uri) => {
    const diags = uri2diags.get(uri.fsPath) || vscode.languages.createDiagnosticCollection()
    uri2diags.set(uri.fsPath, diags)
    diags.clear()
    diags.set(
      groupDiagnostic(
        parseGhcidOutput(path.dirname(uri.fsPath), fs.readFileSync(uri.fsPath, 'utf8')).map(x => [x[0], [x[1]]])
      )
    )
  }

  ;(await vscode.workspace.findFiles('**/ghcid.txt')).forEach(onUpdate)
  watcher.onDidCreate(onUpdate)
  watcher.onDidChange(onUpdate)
  watcher.onDidDelete(uri => {
    uri2diags.get(uri.fsPath)?.dispose()
    uri2diags.delete(uri.fsPath)
  })
}

const getWordRange = (document: vscode.TextDocument, position: vscode.Position): vscode.Range | undefined => {
  return document.getWordRangeAtPosition(position, HASKELL_IDENT)
}

const buildCommand = (prefix: string, document: vscode.TextDocument, range: vscode.Range): string => {
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri)
  const file = workspaceFolder ? path.relative(workspaceFolder.uri.fsPath, document.uri.fsPath) : document.uri.fsPath
  const startLine = range.start.line + 1
  const startCol = range.start.character + 1
  const endLine = range.end.line + 1
  const endCol = range.end.character
  return `${prefix} ${file} ${startLine} ${startCol} ${endLine} ${endCol}`
}

const getWorkspaceRoots = (): string[] => {
  return (vscode.workspace.workspaceFolders ?? []).map(folder => folder.uri.fsPath)
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

const vsCodeDisposableToDisposable = (disposable: vscode.Disposable): Disposable => ({
  [Symbol.dispose]: () => disposable.dispose(),
})

let stack: AsyncDisposableStack | undefined = undefined

const connectToServer = async (context: vscode.ExtensionContext): Promise<void> => {
  stack = newContextStack(context)

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
      fromEventPattern<vscode.WorkspaceFoldersChangeEvent>(
        vscode.workspace.onDidChangeWorkspaceFolders,
        (_, disposable: vscode.Disposable) => disposable.dispose()
      )
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

export type ExtApi = {}

export const activate = async (context: vscode.ExtensionContext): Promise<ExtApi> => {
  await connectToServer(context)

  // Pointer to the last running watcher, so we can undo it
  var oldWatcher: fs.FSWatcher = null
  var oldTerminal: vscode.Terminal = null

  let cleanup = () => {
    if (oldWatcher != null) oldWatcher.close()
    oldWatcher = null
    if (oldTerminal != null) oldTerminal.dispose()
    oldTerminal = null
  }
  context.subscriptions.push({ dispose: cleanup })

  let add = (name: string, act: () => fs.FSWatcher) => {
    let dispose = vscode.commands.registerCommand(name, () => {
      try {
        cleanup()
        oldWatcher = act()
      } catch (e) {
        log('Ghcid extension failed in ' + name + ': ' + e)
        throw e
      }
    })
    context.subscriptions.push(dispose)
  }

  add('extension.startGhcid', () => {
    // TODO support multiple roots
    const workspaceRoot = getWorkspaceRoots()[0]
    if (!workspaceRoot) {
      vscode.window.showWarningMessage('You must open a workspace first.')
      return null
    }
    var hash = crypto.createHash('sha256').update(workspaceRoot).digest('hex').substring(0, 20)
    let file = path.join(os.tmpdir(), 'ghcid-' + hash + '.txt')
    context.subscriptions.push({
      dispose: () => {
        try {
          fs.unlinkSync(file)
        } catch (e) {}
      },
    })
    fs.writeFileSync(file, '')

    let ghcidCommand: string = vscode.workspace.getConfiguration('ghcid').get('command')
    let extraArgs: string[] = ['--outputfile=' + file]

    let opts: vscode.TerminalOptions = {
      name: 'ghcid',
      shellPath: os.type().startsWith('Windows') ? 'cmd.exe' : ghcidCommand,
      shellArgs: [...(os.type().startsWith('Windows') ? ['/k', ghcidCommand] : []), ...extraArgs],
    }
    oldTerminal = vscode.window.createTerminal(opts)
    oldTerminal.show()
    return watchOutput(workspaceRoot, file)
  })

  await autoWatch(context)

  return {}
}

export const deactivate = () => {
  stack?.disposeAsync()
}
