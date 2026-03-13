import { defineConfig } from 'tsdown'

export default defineConfig({
  entry: [
    'src/extension.ts',
    'test/runTest.ts',
    'test/extension-test-runner.ts',
    'test/ghcid-client.test.ts',
    'src/timeout.ts',
  ],
  outDir: 'out',
  format: ['cjs'],
  sourcemap: 'inline',
  target: 'node20',
  clean: true,
  dts: false,
  deps: {
    neverBundle: ['vscode'],
    // Bundle runtime deps so the published VSIX doesn't need node_modules.
    alwaysBundle: ['*'],
  },
})
