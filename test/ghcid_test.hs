module Main where

import Test.Tasty
import Language.Haskell.Ghcid.ParserTest (parserTests)
import Language.Haskell.Ghcid.HighLevelTests (highLevelTests)
import Language.Haskell.Ghcid.UtilTest (utilsTests)
import Language.Haskell.Ghcid.PollingTest (pollingTest)

main::IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ utilsTests
  , parserTests
  , highLevelTests
  , pollingTest
  ]
