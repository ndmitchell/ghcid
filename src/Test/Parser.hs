-- | Test the message parser
module Test.Parser(parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Ghcid.Parser
import Language.Haskell.Ghcid.Types


parserTests :: TestTree
parserTests = testGroup "Parser tests"
    [testParseShowModules
    ,testParseLoad
    ,testParseLoadGhc82
    ,testParseLoadSpans
    ,testParseLoadCycles
    ,testParseLoadCyclesSelf
    ,testParseLoadEscapeCodes
    ,testMissingFile
    ]

testParseShowModules :: TestTree
testParseShowModules = testCase "Show Modules" $ parseShowModules
    ["Main             ( src/Main.hs, interpreted )"
    ,"AI.Neural.WiscDigit ( src/AI/Neural/WiscDigit.hs, interpreted )"
    ] @?=
    [("Main","src/Main.hs")
    ,("AI.Neural.WiscDigit","src/AI/Neural/WiscDigit.hs")
    ]

testParseLoad :: TestTree
testParseLoad = testCase "Load Parsing" $ parseLoad
    ["[1 of 2] Compiling GHCi             ( GHCi.hs, interpreted )"
    ,"GHCi.hs:70:1: Parse error: naked expression at top level"
    ,"GHCi.hs:72:13:"
    ,"    No instance for (Num ([String] -> [String]))"
    ,"      arising from the literal `1'"
    ,"    Possible fix:"
    ,"      add an instance declaration for (Num ([String] -> [String]))"
    ,"    In the expression: 1"
    ,"    In an equation for `parseLoad': parseLoad = 1"
    ,"GHCi.hs:81:1: Warning: Defined but not used: `foo'"
    ,"C:\\GHCi.hs:82:1: warning: Defined but not used: \8216foo\8217" -- GHC 7.12 uses lowercase
    ,"src\\Haskell.hs:4:23:"
    ,"    Warning: {-# SOURCE #-} unnecessary in import of  `Boot'"
    ,"src\\Boot.hs-boot:2:8:"
    ,"    File name does not match module name:"
    ,"    Saw: `BootX'"
    ,"    Expected: `Boot'"
    ] @?=
    [Loading "GHCi" "GHCi.hs"
    ,Message {loadSeverity = Error, loadFile = "GHCi.hs", loadFilePos = (70,1), loadMessage = ["GHCi.hs:70:1: Parse error: naked expression at top level"]}
    ,Message {loadSeverity = Error, loadFile = "GHCi.hs", loadFilePos = (72,13), loadMessage = ["GHCi.hs:72:13:","    No instance for (Num ([String] -> [String]))","      arising from the literal `1'","    Possible fix:","      add an instance declaration for (Num ([String] -> [String]))","    In the expression: 1","    In an equation for `parseLoad': parseLoad = 1"]}
    ,Message {loadSeverity = Warning, loadFile = "GHCi.hs", loadFilePos = (81,1), loadMessage = ["GHCi.hs:81:1: Warning: Defined but not used: `foo'"]}
    ,Message {loadSeverity = Warning, loadFile = "C:\\GHCi.hs", loadFilePos = (82,1), loadMessage = ["C:\\GHCi.hs:82:1: warning: Defined but not used: \8216foo\8217"]}
    ,Message {loadSeverity = Warning, loadFile = "src\\Haskell.hs", loadFilePos = (4,23), loadMessage = ["src\\Haskell.hs:4:23:","    Warning: {-# SOURCE #-} unnecessary in import of  `Boot'"]}
    ,Message {loadSeverity = Error, loadFile = "src\\Boot.hs-boot", loadFilePos = (2,8), loadMessage = ["src\\Boot.hs-boot:2:8:","    File name does not match module name:","    Saw: `BootX'","    Expected: `Boot'"]}
    ]

testParseLoadGhc82 :: TestTree
testParseLoadGhc82 = testCase "GHC 8.2 Load Parsing" $ parseLoad
    ["[18 of 24] Compiling Physics ( Physics.hs, interpreted )"
    ,"Physics.hs:30:18: error: parse error on input ‘^*’"
    ,"   |"
    ,"30 |           dx = ' ^* delta"
    ,"   |                  ^^"
    ,"Loaded GHCi configuration from C:\\Neil\\ghcid\\.ghci"
    ] @?=
    [Loading "Physics" "Physics.hs"
    ,Message {loadSeverity = Error, loadFile = "Physics.hs", loadFilePos = (30,18), loadMessage = ["Physics.hs:30:18: error: parse error on input ‘^*’" ,"   |" ,"30 |           dx = ' ^* delta" ,"   |                  ^^"]}
    ,LoadConfig "C:\\Neil\\ghcid\\.ghci"
    ]

testMissingFile = testCase "Starting ghci with a non-existent filename" $ parseLoad
    ["<no location info>: error: can't find file: bob.hs"
    ] @?=
    []

testParseLoadCyclesSelf = testCase "Module cycle with itself" $ parseLoad
    ["Module imports form a cycle:"
    ,"  module `Language.Haskell.Ghcid.Parser' (src\\Language\\Haskell\\Ghcid\\Parser.hs) imports itself"
    ] @?=
    [Message {loadSeverity = Error, loadFile = "", loadFilePos = (0,0), loadMessage = ["Module imports form a cycle:","  module `Language.Haskell.Ghcid.Parser' (src\\Language\\Haskell\\Ghcid\\Parser.hs) imports itself"]}
    ,Message {loadSeverity = Error, loadFile = "src\\Language\\Haskell\\Ghcid\\Parser.hs", loadFilePos = (0,0), loadMessage = []}]

testParseLoadCycles = testCase "Module cycle" $ parseLoad
    ["[ 4 of 13] Compiling Language.Haskell.Ghcid.Parser ( src\\Language\\Haskell\\Ghcid\\Parser.hs, interpreted )"
    ,"Module imports form a cycle:"
    ,"         module `Language.Haskell.Ghcid.Util' (src\\Language\\Haskell\\Ghcid\\Util.hs)"
    ,"        imports `Language.Haskell.Ghcid' (src\\Language\\Haskell\\Ghcid.hs)"
    ,"  which imports `Language.Haskell.Ghcid.Util' (src\\Language\\Haskell\\Ghcid\\Util.hs)"
    ] @?=
    [Loading {loadModule = "Language.Haskell.Ghcid.Parser", loadFile = "src\\Language\\Haskell\\Ghcid\\Parser.hs"}
    ,Message {loadSeverity = Error, loadFile = "", loadFilePos = (0,0), loadMessage = ["Module imports form a cycle:","         module `Language.Haskell.Ghcid.Util' (src\\Language\\Haskell\\Ghcid\\Util.hs)","        imports `Language.Haskell.Ghcid' (src\\Language\\Haskell\\Ghcid.hs)","  which imports `Language.Haskell.Ghcid.Util' (src\\Language\\Haskell\\Ghcid\\Util.hs)"]}
    ,Message {loadSeverity = Error, loadFile = "src\\Language\\Haskell\\Ghcid\\Util.hs", loadFilePos = (0,0), loadMessage = []}
    ,Message {loadSeverity = Error, loadFile = "src\\Language\\Haskell\\Ghcid.hs", loadFilePos = (0,0), loadMessage = []}]

testParseLoadEscapeCodes = testCase "Escape codes as enabled by -fdiagnostics-color=always" $ parseLoad
    ["\ESC[;1msrc\\Language\\Haskell\\Ghcid\\Types.hs:11:1: \ESC[;1m\ESC[35mwarning:\ESC[0m\ESC[0m\ESC[;1m [\ESC[;1m\ESC[35m-Wunused-imports\ESC[0m\ESC[0m\ESC[;1m]\ESC[0m\ESC[0m\ESC[;1m"
    ,"    The import of `Data.Data' is redundant"
    ,"      except perhaps to import instances from `Data.Data'"
    ,"    To import instances alone, use: import Data.Data()\ESC[0m\ESC[0m"
    ,"\ESC[;1m\ESC[34m   |\ESC[0m\ESC[0m"
    ,"\ESC[;1m\ESC[34m11 |\ESC[0m\ESC[0m \ESC[;1m\ESC[35mimport Data.Data\ESC[0m\ESC[0m"
    ,"\ESC[;1m\ESC[34m   |\ESC[0m\ESC[0m\ESC[;1m\ESC[35m ^^^^^^^^^^^^^^^^\ESC[0m\ESC[0m"
    ,"\ESC[0m\ESC[0m\ESC[0m"
    ,"\ESC[;1msrc\\Language\\Haskell\\Ghcid\\Util.hs:11:1: \ESC[;1m\ESC[31merror:\ESC[0m\ESC[0m\ESC[;1m\ESC[0m\ESC[0m\ESC[;1m"
    ,"    Could not find module `Language.Haskell.Ghcid.None'\ESC[0m\ESC[0m"
    ,"\ESC[;1m\ESC[34m   |\ESC[0m\ESC[0m"
    ,"\ESC[;1m\ESC[34m11 |\ESC[0m\ESC[0m \ESC[;1m\ESC[31mimport Language.Haskell.Ghcid.None\ESC[0m\ESC[0m"
    ,"\ESC[;1m\ESC[34m   |\ESC[0m\ESC[0m\ESC[;1m\ESC[31m ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\ESC[0m\ESC[0m"
    ,"\ESC[0m\ESC[0m\ESC[0m"
    ] @?=
    [Message {loadSeverity = Warning, loadFile = "src\\Language\\Haskell\\Ghcid\\Types.hs", loadFilePos = (11,1), loadMessage = ["\ESC[;1msrc\\Language\\Haskell\\Ghcid\\Types.hs:11:1: \ESC[;1m\ESC[35mwarning:\ESC[0m\ESC[0m\ESC[;1m [\ESC[;1m\ESC[35m-Wunused-imports\ESC[0m\ESC[0m\ESC[;1m]\ESC[0m\ESC[0m\ESC[;1m","    The import of `Data.Data' is redundant","      except perhaps to import instances from `Data.Data'","    To import instances alone, use: import Data.Data()\ESC[0m\ESC[0m","\ESC[;1m\ESC[34m   |\ESC[0m\ESC[0m","\ESC[;1m\ESC[34m11 |\ESC[0m\ESC[0m \ESC[;1m\ESC[35mimport Data.Data\ESC[0m\ESC[0m","\ESC[;1m\ESC[34m   |\ESC[0m\ESC[0m\ESC[;1m\ESC[35m ^^^^^^^^^^^^^^^^\ESC[0m\ESC[0m"]}
    ,Message {loadSeverity = Error, loadFile = "src\\Language\\Haskell\\Ghcid\\Util.hs", loadFilePos = (11,1), loadMessage = ["\ESC[;1msrc\\Language\\Haskell\\Ghcid\\Util.hs:11:1: \ESC[;1m\ESC[31merror:\ESC[0m\ESC[0m\ESC[;1m\ESC[0m\ESC[0m\ESC[;1m","    Could not find module `Language.Haskell.Ghcid.None'\ESC[0m\ESC[0m","\ESC[;1m\ESC[34m   |\ESC[0m\ESC[0m","\ESC[;1m\ESC[34m11 |\ESC[0m\ESC[0m \ESC[;1m\ESC[31mimport Language.Haskell.Ghcid.None\ESC[0m\ESC[0m","\ESC[;1m\ESC[34m   |\ESC[0m\ESC[0m\ESC[;1m\ESC[31m ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\ESC[0m\ESC[0m"]}]

testParseLoadSpans :: TestTree
testParseLoadSpans = testCase "Load Parsing when -ferror-spans is enabled" $ parseLoad
    ["[1 of 2] Compiling GHCi             ( GHCi.hs, interpreted )"
    ,"GHCi.hs:70:1-2: Parse error: naked expression at top level"
    ,"GHCi.hs:72:13-14:"
    ,"    No instance for (Num ([String] -> [String]))"
    ,"      arising from the literal `1'"
    ,"    Possible fix:"
    ,"      add an instance declaration for (Num ([String] -> [String]))"
    ,"    In the expression: 1"
    ,"    In an equation for `parseLoad': parseLoad = 1"
    ,"GHCi.hs:81:1-15: Warning: Defined but not used: `foo'"
    ,"C:\\GHCi.hs:82:1-17: warning: Defined but not used: \8216foo\8217" -- GHC 7.12 uses lowercase
    ,"src\\Haskell.hs:4:23-24:"
    ,"    Warning: {-# SOURCE #-} unnecessary in import of  `Boot'"
    ,"src\\Boot.hs-boot:2:8-5:"
    ,"    File name does not match module name:"
    ,"    Saw: `BootX'"
    ,"    Expected: `Boot'"
    ,"/src/TrieSpec.hs:(192,7)-(193,76): Warning:"
    ,"    A do-notation statement discarded a result of type ‘[()]’"
    ] @?=
    [Loading "GHCi" "GHCi.hs"
    ,Message {loadSeverity = Error, loadFile = "GHCi.hs", loadFilePos = (70,1), loadMessage = ["GHCi.hs:70:1-2: Parse error: naked expression at top level"]}
    ,Message {loadSeverity = Error, loadFile = "GHCi.hs", loadFilePos = (72,13), loadMessage = ["GHCi.hs:72:13-14:","    No instance for (Num ([String] -> [String]))","      arising from the literal `1'","    Possible fix:","      add an instance declaration for (Num ([String] -> [String]))","    In the expression: 1","    In an equation for `parseLoad': parseLoad = 1"]}
    ,Message {loadSeverity = Warning, loadFile = "GHCi.hs", loadFilePos = (81,1), loadMessage = ["GHCi.hs:81:1-15: Warning: Defined but not used: `foo'"]}
    ,Message {loadSeverity = Warning, loadFile = "C:\\GHCi.hs", loadFilePos = (82,1), loadMessage = ["C:\\GHCi.hs:82:1-17: warning: Defined but not used: \8216foo\8217"]}
    ,Message {loadSeverity = Warning, loadFile = "src\\Haskell.hs", loadFilePos = (4,23), loadMessage = ["src\\Haskell.hs:4:23-24:","    Warning: {-# SOURCE #-} unnecessary in import of  `Boot'"]}
    ,Message {loadSeverity = Error, loadFile = "src\\Boot.hs-boot", loadFilePos = (2,8), loadMessage = ["src\\Boot.hs-boot:2:8-5:","    File name does not match module name:","    Saw: `BootX'","    Expected: `Boot'"]}
    ,Message {loadSeverity = Warning, loadFile = "/src/TrieSpec.hs", loadFilePos = (192,7), loadMessage = ["/src/TrieSpec.hs:(192,7)-(193,76): Warning:","    A do-notation statement discarded a result of type ‘[()]’"]}
    ]
