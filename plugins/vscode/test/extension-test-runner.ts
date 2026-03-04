import { run as runExtensionTests } from './extension.test'

export async function run(): Promise<void> {
  await runExtensionTests()
}
