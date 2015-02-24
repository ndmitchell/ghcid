-- | Test utility functions
module Language.Haskell.Ghcid.UtilTest 
  ( utilsTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Ghcid.Util

utilsTests :: TestTree
utilsTests=testGroup "Utility tests"
  [ dropPrefixTests
  , chunksOfWordTests
  ]

dropPrefixTests :: TestTree
dropPrefixTests = testGroup "dropPrefix"
  [ testCase "Prefix not found" $ dropPrefixRepeatedly "prefix" "string"  @?= "string"
  , testCase "Empty prefix" $ dropPrefixRepeatedly "" "string" @?= "string"
  , testCase "Prefix found once" $ dropPrefixRepeatedly "str" "string" @?= "ing"
  , testCase "Prefix found twice" $ dropPrefixRepeatedly "str" "strstring" @?= "ing"
  ]

chunksOfWordTests :: TestTree
chunksOfWordTests = testGroup "chunksOfWord"
  [ testCase "Max 0" $ chunksOfWord 4 0 "ab cd efgh" @?= ["ab c","d ef","gh"]
  , testCase "Max 2" $ chunksOfWord 4 2 "ab cd efgh" @?= ["ab ","cd ","efgh"]
  ]
