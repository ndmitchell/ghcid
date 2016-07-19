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
    ] @?=
    [Loading "GHCi" "GHCi.hs"
    ,Message {loadSeverity = Error, loadFile = "GHCi.hs", loadFilePos = (70,1), loadMessage = ["GHCi.hs:70:1: Parse error: naked expression at top level"]}
    ,Message {loadSeverity = Error, loadFile = "GHCi.hs", loadFilePos = (72,13), loadMessage = ["GHCi.hs:72:13:","    No instance for (Num ([String] -> [String]))","      arising from the literal `1'","    Possible fix:","      add an instance declaration for (Num ([String] -> [String]))","    In the expression: 1","    In an equation for `parseLoad': parseLoad = 1"]}
    ,Message {loadSeverity = Warning, loadFile = "GHCi.hs", loadFilePos = (81,1), loadMessage = ["GHCi.hs:81:1: Warning: Defined but not used: `foo'"]}
    ,Message {loadSeverity = Warning, loadFile = "C:\\GHCi.hs", loadFilePos = (82,1), loadMessage = ["C:\\GHCi.hs:82:1: warning: Defined but not used: \8216foo\8217"]}
    ,Message {loadSeverity = Warning, loadFile = "src\\Haskell.hs", loadFilePos = (4,23), loadMessage = ["src\\Haskell.hs:4:23:","    Warning: {-# SOURCE #-} unnecessary in import of  `Boot'"]}
    ]
