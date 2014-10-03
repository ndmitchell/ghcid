{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Ghcid.ScriptedTest (scriptedTest) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.FilePath
import System.Directory
import System.Exit
import System.IO
import System.Process

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.GhcidProgram
import Language.Haskell.Ghcid.Util

scriptedTest :: TestTree
scriptedTest  = testCase "Scripted Test" $ do
    hSetBuffering stdout NoBuffering
    tdir <- fmap (</> ".ghcid") getTemporaryDirectory
    try_ $ removeDirectoryRecursive tdir
    createDirectoryIfMissing True tdir
    withCurrentDirectory tdir $ do
        ref <- newEmptyMVar
        let require predi = do
                res <- takeMVarDelay ref 5
                predi res
                outStr "."
                sleep 1
        t <- myThreadId
        writeFile "Main.hs" "main = print 1"
        writeFile ".ghci" ":set -fwarn-unused-binds \n:load Main"
        -- otherwise GHC warns about .ghci being accessible by others
        try_ $ system "chmod og-w . .ghci"
        forkIO $ handle (\(e :: SomeException) -> throwTo t e) $ do
            require requireAllGood
            testScript require
            outStrLn "\nSuccess"
            throwTo t ExitSuccess
        runGhcid "ghci" 50 $ \msg -> unless (["Reloading..."] `isPrefixOf` msg) $
            putMVarNow ref msg

putMVarNow :: MVar a -> a -> IO ()
putMVarNow ref x = do
    b <- tryPutMVar ref x
    unless b $ error "Had a message and a new one arrived"

takeMVarDelay :: MVar a -> Double -> IO a
takeMVarDelay _ i | i <= 0 = error "timed out"
takeMVarDelay x i = do
    b <- tryTakeMVar x
    case b of
        Nothing -> sleep 0.1 >> takeMVarDelay x (i-0.1)
        Just v -> return v

--(===) :: (Eq a, Show a) => a -> a -> IO ()
--(===) a b = unless (a == b) $ error $ "Mismatch\nLHS: " ++ show a ++ "\nRHS: " ++ show b

requireAllGood :: [String] -> IO ()
requireAllGood got = filter (not . null) got @?= ["All good"]

requireNonIndents :: [String] -> [String] -> IO ()
requireNonIndents want got = [x | x@(c:_) <- got, not $ isSpace c] @?= want

requirePrefix :: [String] -> [String] -> IO ()
requirePrefix want got = take (length want) got @?= want


---------------------------------------------------------------------
-- ACTUAL TEST SUITE

testScript :: (([String] -> IO ()) -> IO ()) -> IO ()
testScript require = do
    writeFile "Main.hs" "x"
    require $ requireNonIndents ["Main.hs:1:1: Parse error: naked expression at top level"]
    writeFile "Util.hs" "module Util where"
    writeFile "Main.hs" "import Util\nmain = print 1"
    require requireAllGood
    writeFile "Util.hs" "module Util where\nx"
    require $ requireNonIndents ["Util.hs:2:1: Parse error: naked expression at top level"]
    writeFile "Util.hs" "module Util() where\nx = 1"
    require $ requireNonIndents ["Util.hs:2:1: Warning: Defined but not used: `x'"]

    -- check warnings persist properly
    writeFile "Main.hs" "import Util\nx"
    require $ requireNonIndents ["Main.hs:2:1: Parse error: naked expression at top level"
                                ,"Util.hs:2:1: Warning: Defined but not used: `x'"]
    writeFile "Main.hs" "import Util\nmain = print 2"
    require $ requireNonIndents ["Util.hs:2:1: Warning: Defined but not used: `x'"]
    writeFile "Main.hs" "main = print 3"
    require requireAllGood
    writeFile "Main.hs" "import Util\nmain = print 4"
    require $ requireNonIndents ["Util.hs:2:1: Warning: Defined but not used: `x'"]
    writeFile "Util.hs" "module Util where"
    require requireAllGood

    -- check renaming files works
    -- note that due to GHC bug #9648 we can't save down a new file
    renameFile "Util.hs" "Util2.hs"
    require $ \s -> do requirePrefix ["Main.hs:1:8:","    Could not find module `Util'"] s
                       requireNonIndents ["Main.hs:1:8:"] s
    renameFile "Util2.hs" "Util.hs"
    require requireAllGood
