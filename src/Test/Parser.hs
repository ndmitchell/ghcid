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
    ,testParseLoadSpans
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

-- | Test when error messages include spans (-ferror-spans)
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
