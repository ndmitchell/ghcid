
module Test(main) where

import Test.Tasty
import System.IO
import System.Console.CmdArgs

import Test.Util
import Test.Parser
import Test.API
import Test.Ghcid

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    setVerbosity Loud
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" $ take 3 -- TEMPORARY
    [utilsTests
    ,parserTests
    ,apiTests
    ,ghcidTest
    ]
