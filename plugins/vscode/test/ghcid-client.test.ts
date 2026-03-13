import * as path from 'path'
import { safely, startGhcid, startGhcidClient, signalToPromise, green, red, blue } from '../src/util'
import expect from 'expect'
import { prettyErrorTree } from 'pretty-error-tree'

const test = async ({ log, signal }: { log: ((...args: any[]) => void) | undefined; signal: AbortSignal }) => {
  const repoRoot = path.resolve(process.cwd(), '../..')
  const workspaceRoot = path.join(repoRoot, 'plugins/vscode/test-projects/foo')

  log('Starting ghcid socket client...')
  const ready = Promise.withResolvers<void>()
  await using client = await startGhcidClient({
    workspaceRoot,
    onDiagnostics: (_workspaceRoot, diag) => {
      if (diag !== '') ready.resolve()
    },
    log,
  })(signal)

  log('Starting ghcid...')
  await using ghcid = await startGhcid({ workspaceRoot, log })(signal)

  log('Waiting for diagnostics to confirm client is connected...')
  await Promise.race([ready.promise, signalToPromise(client.signal)])

  log('Sending :type-at command')
  const { output } = await client.request(path.join(workspaceRoot, 'Lib.hs'), ':type-at Lib.hs 5 34 5 38')

  expect(output).toMatch(/:: Char/)
}

const main = async (signal: AbortSignal) => {
  const verbose = process.argv.includes('-v')
  const log: typeof console.log = (...args) => {
    if (verbose) console.log(blue(`[test]`), ...args)
  }
  try {
    await test({ log, signal })
    console.log(`Test ${green('passed')}.`)
  } catch (err) {
    delete err['matcherResult']
    console.log(prettyErrorTree(err) + '\n')
    process.exitCode = 1
    console.log(`Test ${red('failed')}.`)
  }
}

safely(main)
