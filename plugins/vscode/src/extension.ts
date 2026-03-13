'use strict'
import * as vscode from 'vscode'
import * as path from 'path'
import * as fs from 'fs'
import * as os from 'os'
import * as crypto from 'crypto'
import * as util from 'util'
import { distinctUntilChanged, fromEventPattern, map, pairwise, startWith } from 'rxjs'
import { Resource, startGhcidMultiClient } from './util'

const outputChannel = vscode.window.createOutputChannel('ghcid')

const isTest = process.env.VSCODE_TEST === 'true'

const log = (...args: any[]) => {
  const line = util.format(...args)
  outputChannel.appendLine(util.stripVTControlCharacters(line))
  if (isTest) console.log(`[ext] ${line}`)
}

function pair<a, b>(a: a, b: b): [a, b] {
  return [a, b]
}

// Regex matching Haskell identifiers including qualified names (e.g. Data.Map.lookup)
// and backtick-quoted infix usage (e.g. `elem`)
const HASKELL_IDENT =
  /`(?:[A-Z][a-zA-Z0-9_']*\.)*[a-zA-Z_][a-zA-Z0-9_']*`|(?:[A-Z][a-zA-Z0-9_']*\.)*[a-zA-Z_][a-zA-Z0-9_']*/

export function parseGhcidOutput(dir: string, s: string): [vscode.Uri, vscode.Diagnostic][] {
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
    return [pair(uri, new vscode.Diagnostic(range, bodyLines.join('\n'), severity))]
  })
}

function groupDiagnostic(xs: [vscode.Uri, vscode.Diagnostic[]][]): [vscode.Uri, vscode.Diagnostic[]][] {
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
    .map(x => pair(x[1], x[2]))
}

function watchOutput(root: string, file: string): fs.FSWatcher {
  let d = vscode.languages.createDiagnosticCollection('ghcid')
  let go = () => {
    d.clear()
    d.set(groupDiagnostic(parseGhcidOutput(root, fs.readFileSync(file, 'utf8')).map(x => pair(x[0], [x[1]]))))
  }
  let watcher = fs.watch(file, go)
  go()
  return watcher
}

async function autoWatch(context: vscode.ExtensionContext) {
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
        parseGhcidOutput(path.dirname(uri.fsPath), fs.readFileSync(uri.fsPath, 'utf8')).map(x => pair(x[0], [x[1]]))
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

function getWordRange(document: vscode.TextDocument, position: vscode.Position): vscode.Range | undefined {
  return document.getWordRangeAtPosition(position, HASKELL_IDENT)
}

function buildCommand(prefix: string, document: vscode.TextDocument, range: vscode.Range): string {
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri)
  const file = workspaceFolder ? path.relative(workspaceFolder.uri.fsPath, document.uri.fsPath) : document.uri.fsPath
  const startLine = range.start.line + 1
  const startCol = range.start.character + 1
  const endLine = range.end.line + 1
  const endCol = range.end.character
  return `${prefix} ${file} ${startLine} ${startCol} ${endLine} ${endCol}`
}

function getWorkspaceRoots(): string[] {
  return (vscode.workspace.workspaceFolders ?? []).map(folder => folder.uri.fsPath)
}

function toLocationUri(workspaceRoot: string, file: string): vscode.Uri {
  return path.isAbsolute(file) || !workspaceRoot
    ? vscode.Uri.file(file)
    : vscode.Uri.file(path.join(workspaceRoot, file))
}

function parseLocation(workspaceRoot: string, line: string): vscode.Location | undefined {
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

let ghcidClient:
  | Resource<{
      addDir: (workspaceRoot: string) => Promise<void>
      removeDir: (workspaceRoot: string) => Promise<void>
      request: (file: string, command: string) => Promise<{ workspaceRoot: string; output: string }>
    }>
  | undefined
let extensionContext: vscode.ExtensionContext | undefined
let languageProvidersRegistered = false
const socketDiagnostics = new Map<string, vscode.DiagnosticCollection>()
let socketTmpRootForTest: string | undefined

const HASKELL_DOCUMENT_SELECTOR: vscode.DocumentSelector = [
  { scheme: 'file', language: 'haskell' },
  { scheme: 'file', pattern: '**/*.hs' },
  { scheme: 'file', pattern: '**/*.lhs' },
  { scheme: 'file', pattern: '**/*.hs-boot' },
]

function workspaceRootsChanges() {
  return fromEventPattern<void>(
    handler => vscode.workspace.onDidChangeWorkspaceFolders(() => handler()),
    (_, disposable: vscode.Disposable) => disposable.dispose()
  ).pipe(
    startWith(undefined),
    map(() => getWorkspaceRoots()),
    distinctUntilChanged((a, b) => a.length === b.length && a.every((root, i) => root === b[i])),
    startWith([] as string[]),
    pairwise()
  )
}

async function connectToServer(context: vscode.ExtensionContext) {
  const controller = new AbortController()
  ghcidClient = await startGhcidMultiClient({
    onDiagnostics: (workspaceRoot, diagnostics) => {
      const diagnosticsForRoot = groupDiagnostic(
        parseGhcidOutput(workspaceRoot, diagnostics).map(x => pair(x[0], [x[1]]))
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
  ghcidClient.signal.addEventListener(
    'abort',
    () => {
      for (const collection of socketDiagnostics.values()) collection.clear()
    },
    { once: true }
  )
  for (const workspaceRoot of getWorkspaceRoots()) {
    await ghcidClient.addDir(workspaceRoot)
  }
  const workspaceSubscription = workspaceRootsChanges().subscribe({
    next: async ([previousRoots, nextRoots]) => {
      const previous = new Set(previousRoots)
      const next = new Set(nextRoots)
      for (const workspaceRoot of previousRoots) {
        if (!next.has(workspaceRoot)) {
          await ghcidClient?.removeDir(workspaceRoot)
          socketDiagnostics.get(workspaceRoot)?.dispose()
          socketDiagnostics.delete(workspaceRoot)
        }
      }
      for (const workspaceRoot of nextRoots) {
        if (!previous.has(workspaceRoot)) await ghcidClient?.addDir(workspaceRoot)
      }
    },
    error: err => {
      log(`socket client workspace sync failed: ${err instanceof Error ? err.message : String(err)}`)
    },
  })
  context.subscriptions.push({
    dispose: () => {
      workspaceSubscription.unsubscribe()
      void ghcidClient?.[Symbol.asyncDispose]()
      ghcidClient = undefined
      for (const collection of socketDiagnostics.values()) collection.dispose()
      socketDiagnostics.clear()
    },
  })

  if (!languageProvidersRegistered) {
    languageProvidersRegistered = true
    context.subscriptions.push(
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

    context.subscriptions.push(
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

    context.subscriptions.push(
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
  }
}

export type ExtApi = {}

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext): Promise<ExtApi> {
  extensionContext = context
  // The command has been defined in the package.json file
  // Now provide the implementation of the command with  registerCommand
  // The commandId parameter must match the command field in package.json
  connectToServer(context)

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

export function deactivate() {
  for (const collection of socketDiagnostics.values()) collection.dispose()
  socketDiagnostics.clear()
  ghcidClient?.[Symbol.asyncDispose]()
  ghcidClient = undefined
}

export function setSocketTmpRootForTest(root: string | undefined) {
  socketTmpRootForTest = root
}
