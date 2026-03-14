import * as os from 'os'
import { createHash } from 'crypto'
import split2 from 'split2'
import { once } from 'events'
import * as path from 'path'
import * as fs from 'fs/promises'
import * as fsSync from 'fs'
import { CallSiteObject } from 'util'
import { fileURLToPath } from 'node:url'
import { prettyErrorTree } from 'pretty-error-tree'
import type { ErrorExtra, Frame } from 'pretty-error-tree'
import type { Resource as StrayResource } from 'why-is-node-running' with { 'resolution-mode': 'import' }
import timers from 'timers/promises'
import net from 'net'
import EventEmitter from 'node:events'
import { spawn } from 'child_process'
import { SpawnOptions } from 'node:child_process'
import nanoSpawn from 'nano-spawn'
import whyIsNodeRunning, { ASYNC_TYPE_INFO } from 'why-is-node-running'

export const repoRoot = path.resolve(path.join(process.cwd(), '..', '..'))

export const startGhcid =
  ({ workspaceRoot, log }: { workspaceRoot: string; log?: (...args: any[]) => void }): CreateResource<unknown> =>
  async signal => {
    log?.('Building ghcid...')
    await nanoSpawn('cabal', ['build', 'exe:ghcid'], { cwd: repoRoot, timeout: 600_000, stdio: 'inherit' })
    const bin = await nanoSpawn('cabal', ['list-bin', 'exe:ghcid'], { cwd: repoRoot, timeout: 10_000 })

    log?.('Starting ghcid...')
    return await startProcess({
      cmd: bin.stdout.trim(),
      args: [],
      opts: {
        cwd: workspaceRoot,
        killSignal: 'SIGINT', // Simulates ctrl-c in the terminal
      },
    })(signal)
  }

export const startGhcidClient = ({
  workspaceRoot,
  onDiagnostics,
  log,
}: {
  workspaceRoot: string
  onDiagnostics?: (workspaceRoot: string, payload: string) => void
  log?: (...args: any[]) => void
}): CreateResource<{
  request: (file: string, command: string) => Promise<{ workspaceRoot: string; output: string }>
}> => {
  const shortHash = createHash('sha256').update(workspaceRoot, 'utf8').digest('base64url').slice(0, 8)
  const socketRoot = os.type().startsWith('Windows') ? os.tmpdir() : path.join('/tmp', 'ghcid')
  const socketDir = path.join(socketRoot, shortHash)
  const socketFileName = 'server.sock'
  const serverSocketPath = path.join(socketDir, socketFileName)

  let pending: PromiseWithResolvers<string> | undefined = undefined
  let socket: net.Socket | undefined = undefined
  let socketFileChange = new AbortController()
  const onSocketFileChanged = () => socketFileChange.abort()
  const rejectPending = (reason: unknown) => {
    pending?.reject(reason)
    pending = undefined
  }

  const handleLine = (line: string) => {
    try {
      const msg = JSON.parse(line)
      switch (msg.type) {
        case 'diag':
          log?.('Received diag:', gray(JSON.stringify(msg.payload)))
          onDiagnostics?.(workspaceRoot, msg.payload)
          break
        case 'stdout':
          if (!pending) log?.(`Received stdout with no pending request: ${gray(JSON.stringify(msg.payload))}`)
          else log?.(`Received stdout: ${gray(JSON.stringify(msg.payload))}`)
          pending?.resolve(msg.payload)
          pending = undefined
          break
        case 'err':
          if (!pending) log?.(`Received err with no pending request: ${gray(JSON.stringify(msg.payload))}`)
          else log?.(`Received err: ${gray(JSON.stringify(msg.payload))}`)
          pending?.reject(new Error(msg.payload))
          pending = undefined
          break
        default:
          log?.(`Received message with unknown type: ${gray(line)}`)
          break
      }
    } catch {
      log?.(`Failed to parse message: ${gray(line)}`)
    }
  }

  const connectAndListen = async (signal: AbortSignal): Promise<never> => {
    log?.(`${gray(serverSocketPath)} connection...`)
    socket = net.createConnection({ path: serverSocketPath, signal })
    socket.once('connect', () => log?.(`${gray(serverSocketPath)} connection...connected`))
    socket.once('close', () => rejectPending('socket closed'))
    const origin = new Error('Socket created here')
    using _ = {
      [Symbol.dispose]() {
        socket = undefined
      },
    }
    const done = once(socket, 'close')
    done.catch(() => {})
    socket.pipe(split2()).on('data', handleLine)
    try {
      await done
      throw new Error('socket closed')
    } catch (err) {
      err.cause = origin
      throw err
    } finally {
      rejectPending(new Error(`socket closed: ${serverSocketPath}`))
      onDiagnostics?.(workspaceRoot, ``)
      log?.(`${gray(serverSocketPath)} connection...closed`)
    }
  }

  const reconnectingSocket = async (signal: AbortSignal): Promise<never> => {
    while (true) {
      try {
        return await retry(connectAndListen)(AbortSignal.any([signal, socketFileChange.signal]))
      } catch (err) {
        if (signal.aborted) throw err
        if (socketFileChange.signal.aborted) {
          socketFileChange = new AbortController()
          continue
        }
      }
    }
  }

  const watcher = async (signal: AbortSignal): Promise<never> => {
    await fs.mkdir(socketDir, { recursive: true })
    log?.(`${gray(serverSocketPath)} watching for changes...`)
    for await (const { filename } of fs.watch(socketDir, { signal })) {
      if (filename === socketFileName) {
        log?.(`${gray(serverSocketPath)} watching for changes...detected change`)
        onSocketFileChanged()
      }
    }
    return undefined as never
  }

  const request = async (_file: string, payload: string): Promise<{ workspaceRoot: string; output: string }> => {
    if (pending) throw new Error(`Cannot send request while another is pending`)
    if (!socket) throw new Error(`Cannot send request while not connected to socket ${serverSocketPath}`)

    using _ = (() => {
      pending = Promise.withResolvers()
      return {
        [Symbol.dispose]() {
          pending = undefined
        },
      }
    })()

    log?.(`Sending stdin: ${gray(JSON.stringify(payload))}`)
    socket.write(JSON.stringify({ type: 'stdin', payload }) + '\n', err => {
      if (err) rejectPending(err)
    })

    return { workspaceRoot, output: await pending.promise }
  }

  return constructCreateResource(async signal => {
    return [{ request }, Promise.allSettled([watcher(signal), reconnectingSocket(signal)])]
  })
}

export const startGhcidMultiClient = ({
  onDiagnostics,
  log,
}: {
  onDiagnostics?: (workspaceRoot: string, payload: string) => void
  log?: (...args: any[]) => void
}): CreateResource<{
  addDir: (workspaceRoot: string) => Promise<void>
  removeDir: (workspaceRoot: string) => Promise<void>
  request: (file: string, command: string) => Promise<{ workspaceRoot: string; output: string }>
}> =>
  constructCreateResource(async signal => {
    const clients = new Map<
      string,
      Resource<{ request: (file: string, command: string) => Promise<{ workspaceRoot: string; output: string }> }>
    >()

    const bestWorkspaceRoot = (file: string): string | undefined => {
      const normalizedFile = path.resolve(file)
      const matches = Array.from(clients.keys()).filter(root => {
        const relative = path.relative(root, normalizedFile)
        return relative === '' || (!relative.startsWith('..') && !path.isAbsolute(relative))
      })
      return matches.sort((a, b) => b.length - a.length)[0]
    }

    const addDir = async (workspaceRoot: string): Promise<void> => {
      const normalizedRoot = path.resolve(workspaceRoot)
      if (clients.has(normalizedRoot)) return
      const client = await startGhcidClient({
        workspaceRoot: normalizedRoot,
        onDiagnostics,
        log,
      })(signal)
      clients.set(normalizedRoot, client)
    }

    const removeDir = async (workspaceRoot: string): Promise<void> => {
      const normalizedRoot = path.resolve(workspaceRoot)
      const client = clients.get(normalizedRoot)
      if (!client) return
      clients.delete(normalizedRoot)
      await client[Symbol.asyncDispose]()
    }

    const request = async (file: string, command: string): Promise<{ workspaceRoot: string; output: string }> => {
      const workspaceRoot = bestWorkspaceRoot(file)
      if (!workspaceRoot) {
        throw new Error(`Cannot route request for file outside tracked workspace folders: ${file}`)
      }
      const client = clients.get(workspaceRoot)
      if (!client) {
        throw new Error(`Cannot route request for file because workspace client is unavailable: ${file}`)
      }
      return await client.request(file, command)
    }

    const done = signalToPromise(signal).finally(async () => {
      await Promise.all(Array.from(clients.values()).map(client => client[Symbol.asyncDispose]()))
      clients.clear()
    })

    return [{ addDir, removeDir, request }, done]
  })

// Runs an abortable until it resolves, exponential backoff between attempts.
const retry =
  <T>(fn: Abortable<T>, delayMs = 1): Abortable<T> =>
  async signal => {
    let currentDelayMs = delayMs
    while (true) {
      try {
        return await fn(signal)
      } catch (err) {
        if (signal.aborted) throw err
        await timers.setTimeout(currentDelayMs, undefined, { signal })
        currentDelayMs *= 10
      }
    }
  }

// spawn for abortables
export const startProcess = ({
  cmd,
  args,
  opts,
}: {
  cmd: string
  args: string[]
  opts: SpawnOptions
}): CreateResource<unknown> =>
  constructCreateResource(async signal => {
    const process = spawn(cmd, args, { ...opts, signal })
    await started(process, signal)
    return [{ process }, closed(process)]
  })

export const constructCreateResource =
  <T>(start: Abortable<[T, Promise<unknown>]>): CreateResource<T> =>
  async signal => {
    const controller = new AbortController()
    const [resource, done] = await start(AbortSignal.any([signal, controller.signal]))
    done.catch(() => {}) // Avoid unhandled rejection

    return {
      async [Symbol.asyncDispose]() {
        await abortAndWait({ promise: done, fallback: undefined, controller })
      },
      signal: promiseToSignal('constructCreateResource: resource is done', done),
      ...resource,
    }
  }

export type Resource<T> = T & AsyncDisposable & { signal: AbortSignal }

export type CreateResource<T> = Abortable<Resource<T>>

export type Abortable<T> = (signal: AbortSignal) => Promise<T>

export const promiseToSignal = (reason: any, p: Promise<unknown>): AbortSignal => {
  const controller = new AbortController()
  p.then(
    v => controller.abort(reason),
    e => controller.abort(e)
  )
  return controller.signal
}

export const signalToPromise = (signal: AbortSignal): Promise<void> =>
  new Promise((_, reject) => {
    if (signal.aborted) reject(signal.reason)
    signal.addEventListener('abort', () => reject(signal.reason), { once: true })
  })

const abortAndWait = async <T, U>({
  promise,
  fallback,
  controller,
}: {
  promise: Promise<T>
  fallback: U
  controller: AbortController
}): Promise<T | U> => {
  const reason = new Error('abort')
  controller.abort(reason)
  try {
    return await promise
  } catch (err) {
    if (err?.name === 'AbortError') return fallback
    else throw err
  }
}

type GetLines = (file: string) => string[] | undefined

const newFileCache = (): GetLines => {
  const cache = new Map<string, string[]>()

  return (file: string): string[] | undefined => {
    let lines = cache.get(file)
    if (!lines) {
      try {
        lines = fsSync.readFileSync(file, 'utf-8').split('\n')
        cache.set(file, lines)
      } catch (err) {
        return undefined
      }
    }
    return lines
  }
}

export const safely = async (main: (signal: AbortSignal) => Promise<void>) => {
  const customWhyIsNodeRunning = async () => {
    const getLines = newFileCache()

    const callSiteObjectToFrame = (call: CallSiteObject, getLines: GetLines): Frame => {
      const file = call.scriptName.startsWith('file://') ? fileURLToPath(call.scriptName) : call.scriptName
      return {
        file,
        line: call.lineNumber,
        column: call.columnNumber,
        sourceLine: getLines(file)?.[call.lineNumber - 1],
        callee: call.functionName,
      }
    }

    whyIsNodeRunning({ error: console.error }, (resources: StrayResource[]) => {
      const resourceToError = (r: StrayResource): Error => {
        const err: ErrorExtra = new Error(`${ASYNC_TYPE_INFO.get(r.type)?.label ?? r.type} is still running`)
        err.name = `Stray resource`
        err.parsedStack = r.stacks.map((s): Frame => callSiteObjectToFrame(s, getLines))
        return err
      }

      const errs = resources.map(resourceToError)
      const msg = 'Stray async resources are keeping Node.js running'
      console.error(prettyErrorTreePadded(new AggregateError(errs, msg)))
    })

    process.exit(1)
  }

  const controller = new AbortController()

  const sigint = Symbol('SIGINT')
  process.on('SIGINT', () => controller.abort(sigint))

  const uncaught = Symbol('uncaught')
  const handleUncaught = async (msg: string, err: any) => {
    if (typeof err === 'object' && uncaught in err) return
    if (err[uncaught] === uncaught) err[uncaught] = uncaught // Mark it so we can ignore rethrow
    addNote({ err, msg: `This error reached the top level via ${msg} without being handled.` })
    console.error(prettyErrorTreePadded(err))
    controller.abort(err)
    process.exitCode = 1
  }
  process.on('uncaughtException', e => handleUncaught('uncaughtException', e))
  process.on('unhandledRejection', e => handleUncaught('unhandledRejection', e))

  try {
    await main(controller.signal)
  } catch (err) {
    if (err === sigint) {
      process.exitCode = 130
    } else {
      handleUncaught('main', err)
    }
  } finally {
    const t = setTimeout(customWhyIsNodeRunning, 100)
    t.unref() // Don't keep the process alive just for this timer
  }
}

export const addNote = <T>({ err, msg }: { err: T; msg: string }): T => {
  if ((err as any).notes === undefined) {
    ;(err as any).notes = [msg]
  } else if (Array.isArray((err as any).notes)) {
    ;(err as any).notes.push(msg)
  } else {
    // If .notes exists but isn't an array, we won't modify it to avoid unexpected mutations
  }
  return err
}

const prettyErrorTreePadded: typeof prettyErrorTree = (err): string => `\n${prettyErrorTree(err)}\n`

const started = (e: EventEmitter, signal?: AbortSignal): Promise<void> =>
  new Promise<void>((resolve, reject) => {
    const onSpawn = () => {
      cleanup()
      resolve()
    }

    const onError = (err: unknown) => {
      cleanup()
      reject(err)
    }

    const onAbort = () => {
      cleanup()
      reject(signal?.reason)
    }

    const cleanup = () => {
      e.off('spawn', onSpawn)
      e.off('error', onError)
      signal?.removeEventListener('abort', onAbort)
    }

    e.once('spawn', onSpawn)
    e.once('error', onError)
    if (signal?.aborted) onAbort()
    else signal?.addEventListener('abort', onAbort, { once: true })
  })

const closed = (e: EventEmitter): Promise<void> =>
  new Promise<void>((resolve, reject) => {
    const onError = (err: unknown) => {
      cleanup()
      reject(err)
    }

    const onClose = () => {
      cleanup()
      resolve()
    }

    const cleanup = () => {
      e.off('error', onError)
      e.off('close', onClose)
    }

    e.on('error', onError)
    e.on('close', onClose)
  })

export const red = (str: string) => `\x1b[31m${str}\x1b[0m`
export const green = (str: string) => `\x1b[32m${str}\x1b[0m`
export const blue = (str: string) => `\x1b[36m${str}\x1b[0m`
export const gray = (str: string) => `\x1b[90m${str}\x1b[0m`
