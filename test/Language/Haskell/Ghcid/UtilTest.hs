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
  [ testCase "Prefix not found" $ dropPrefix "prefix" "string"  @?= "string"
  , testCase "Empty prefix" $ dropPrefix "" "string" @?= "string"
  , testCase "Prefix found once" $ dropPrefix "str" "string" @?= "ing"
  , testCase "Prefix found twice" $ dropPrefix "str" "strstring" @?= "ing"
  ]