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
  ]
  
dropPrefixTests :: TestTree
dropPrefixTests = testGroup "dropPrefix"
  [ testCase "Prefix not found" $ dropPrefixRepeatedly "prefix" "string"  @?= "string"
  , testCase "Empty prefix" $ dropPrefixRepeatedly "" "string" @?= "string"
  , testCase "Prefix found once" $ dropPrefixRepeatedly "str" "string" @?= "ing"
  , testCase "Prefix found twice" $ dropPrefixRepeatedly "str" "strstring" @?= "ing"
  ]
