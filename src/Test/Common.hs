
module Test.Common(disable19650) where

import Test.Tasty
import System.Info
import Data.Version

-- Tests which are disabled due to https://gitlab.haskell.org/ghc/ghc/-/issues/19650
-- readily resetting which packages are loaded
disable19650 :: TestTree -> TestTree
disable19650 x
    | compilerVersion < makeVersion [9] = x
    | otherwise = testGroup "Disabled" []
