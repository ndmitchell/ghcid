import { spawn } from 'child_process'
import * as assert from 'assert/strict'
import { once } from 'events'
import timers from 'timers/promises'

function usage(): never {
  console.error('Usage: timeout <seconds> <command...>')
  process.exit(1)
}

const main = async (signal: AbortSignal): Promise<void> => {
  const [, , secondsArg, ...command] = process.argv
  if (!secondsArg || command.length === 0) usage()

  const timeoutSeconds = Number.parseFloat(secondsArg)
  if (!Number.isFinite(timeoutSeconds) || timeoutSeconds < 0) {
    console.error(`Invalid timeout seconds: ${secondsArg}`)
    process.exit(1)
  }

  const child = spawn(command[0], command.slice(1), {
    stdio: 'inherit',
    detached: process.platform !== 'win32',
  })
  const done = once(child, 'close') as Promise<[number | null, NodeJS.Signals | null]>
  done.catch(() => {}) // avoid unhandled rejection if race loses

  const interruptChild = async () => {
    const pid = child.pid
    if (pid === undefined) assert.fail(`Expected child process to have a PID, but it does not.`)
    process.kill(process.platform === 'win32' ? pid : -pid, 'SIGINT')
    await done
  }

  const timer =
    (ms: number): Abortable =>
    signal =>
      timers.setTimeout(ms, undefined, { signal })

  const controller = new AbortController()
  await Promise.all([
    done
      .then(
        v => {
          if (child.pid !== undefined && process.exitCode === undefined) process.exitCode = child.exitCode
        },
        e => {
          if (e?.code === 'ENOENT') {
            console.log(`${redStr('Command not found')}: ${grayStr(e.path ?? '')}`)
            process.exitCode = 1
          } else if (e?.code === 'EACCES') {
            console.log(`${redStr('Permission denied')}: ${grayStr(e.path ?? '')}`)
            console.log(`Try making it executable: chmod +x ${e.path}`)
            process.exitCode = 1
          }
        }
      )
      .finally(() => controller.abort()),
    timer(timeoutSeconds * 1000)(AbortSignal.any([signal, controller.signal])).then(
      async () => {
        console.log(`${redStr('Timed out')} after ${timeoutSeconds} seconds: ${grayStr(command.join(' '))}`)
        process.exitCode = 124
        await interruptChild()
      },
      async e => {
        if (controller.signal.aborted) {
          // ignore
        } else if (signal.aborted) {
          await interruptChild()
          throw signal.reason
        }
      }
    ),
  ])
}

type Abortable<T = void> = (signal: AbortSignal) => Promise<T>

const safely = async (main: (signal: AbortSignal) => Promise<void>): Promise<void> => {
  const controller = new AbortController()

  const h = () => controller.abort()

  process.on('SIGINT', h)
  process.on('SIGTERM', h)
  process.on('uncaughtException', h)
  process.on('unhandledRejection', h)

  try {
    await main(controller.signal)
  } catch (e) {
    if (controller.signal.aborted) {
      process.exitCode = 130
    } else {
      console.log(e)
      process.exitCode = 1
    }
  }

  process.on('exit', () => process.exit(process.exitCode))
}

const redStr = (str: string) => `$\x1b[31m${str}\x1b[0m`
const grayStr = (str: string) => `\x1b[90m${str}\x1b[0m`

if (require.main === module) {
  safely(main)
}
