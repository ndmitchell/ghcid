import * as path from 'path'
import * as vscode from 'vscode'
import expect from 'expect'

import { type ExtApi, parseGhcidOutput } from '../src/extension'

const parseGhcidOutputTest = () => {
  const tests = [
    {
      src: `
src\\Test.hs:81:11: error:
    * No instance for (Num (IO [String])) arising from a use of \`+'
    * In a stmt of a 'do' block: xs <- getArgs + getArgs
src\\General\\Binary.hs:15:1-22: warning: [-Wunused-imports]
    The import of \`Data.List.Extra' is redundant
src\\General\\Binary.hs:17:1-23: warning: [-Wunused-imports]
    The import of \`Data.Tuple.Extra' is redundant
C:\\src\\Development\\Shake\\Internal\\FileInfo.hs:(15,1)-(16,23): warning: [-Wunused-imports]
    The import of \`GHC.IO.Exception' is redundant
`.replace(/\n/g, '\r\n'),
      want: [
        ['/src/Test.hs', [80, 10, 80, 11], vscode.DiagnosticSeverity.Error],
        ['/src/General/Binary.hs', [14, 0, 14, 22], vscode.DiagnosticSeverity.Warning],
        ['/src/General/Binary.hs', [16, 0, 16, 23], vscode.DiagnosticSeverity.Warning],
        ['/C:/src/Development/Shake/Internal/FileInfo.hs', [14, 0, 15, 23], vscode.DiagnosticSeverity.Warning],
      ],
    },
    {
      src: `
/project/Main.hs:57:35: error:
    Variable not in scope:
      source
        :: Int
   |
57 |         run source srv
   |             ^^^^^^
                `,
      want: [['/project/Main.hs', [56, 12, 56, 18], vscode.DiagnosticSeverity.Error]],
    },
    {
      src: `
app/Main.hs:20:5-13: error: [GHC-88464]
    Variable not in scope: putStrLns :: String -> IO a0
    Suggested fix: Perhaps use ‘putStrLn’ (imported from System.IO)
   |
20 |     putStrLns "Waiting for SIGINT (Ctrl+C)..."
   |     ^^^^^^^^^`,
      want: [['/app/Main.hs', [19, 4, 19, 13], vscode.DiagnosticSeverity.Error]],
    },
    {
      src: `Ghcid has stopped.`,
      want: [],
    },
  ]

  for (const test of tests) {
    const res = parseGhcidOutput('', test.src)
    const got = res.map(x => [
      x[0].path,
      [x[1].range.start.line, x[1].range.start.character, x[1].range.end.line, x[1].range.end.character],
      x[1].severity,
    ])
    expect(got).toEqual(test.want)
  }
}

export async function run(): Promise<void> {
  parseGhcidOutputTest()
}
