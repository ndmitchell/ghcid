
module Test(main) where

import Test.Tasty
import Test.Tasty.Runners (NumThreads(..))
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
-- Several integration tests temporarily change the process-wide current
-- directory, so they must not overlap with each other.
tests = localOption (NumThreads 1) $ testGroup "Tests"
    [utilsTests
    ,parserTests
    ,apiTests
    ,ghcidTest
    ]
