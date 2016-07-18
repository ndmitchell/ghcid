
module Test(main) where

import Test.Tasty
import System.IO

import Test.Util
import Test.Parser
import Test.API
import Test.Ghcid

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [utilsTests
    ,parserTests
    ,apiTests
    ,ghcidTest
    ]
