
module Test(main) where

import Test.Tasty
import Test.Parser
import Test.HighLevel
import Test.Util
import Test.Polling
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [utilsTests
    ,parserTests
    ,pollingTest
    ,highLevelTests
    ]
