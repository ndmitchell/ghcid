import * as path from 'path'
import { runTests } from '@vscode/test-electron'

async function main() {
  try {
    const extensionDevelopmentPath = path.resolve(__dirname, '../..')
    const workspacePath = path.resolve(extensionDevelopmentPath, 'test-projects/foo')
    const extensionTestsPath = path.resolve(__dirname, './extension-test-runner.cjs')

    await runTests({
      extensionDevelopmentPath,
      extensionTestsPath,
      launchArgs: [workspacePath],
      extensionTestsEnv: { VSCODE_TEST: 'true' },
    })
  } catch {
    console.error('Failed to run tests')
    process.exit(1)
  }
}

void main()
