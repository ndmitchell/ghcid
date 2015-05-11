
module Test(main) where

import Test.Tasty
import Test.Parser (parserTests)
import Test.HighLevel (highLevelTests)
import Test.Util (utilsTests)
import Test.Polling (pollingTest)

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ utilsTests
  , parserTests
  , highLevelTests
  , pollingTest
  ]
