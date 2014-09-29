{-# LANGUAGE ScopedTypeVariables #-}

module Test(runTest) where

import Util
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.FilePath
import System.Directory
import System.Exit
import System.IO


runTest :: Bool -> (([String] -> IO ()) -> IO a) -> IO a
runTest False f = f $ outStr . unlines
runTest True  f = do
    hSetBuffering stdout NoBuffering
    tdir <- fmap (</> ".ghcid") getTemporaryDirectory
    try $ removeDirectoryRecursive tdir :: IO (Either SomeException ())
    createDirectoryIfMissing True tdir
    withCurrentDirectory tdir $ do
        ref <- newEmptyMVar
        let require xs = do
                res <- takeMVarDelay ref 5
                let ys = [x | x@(c:_) <- res, not $ isSpace c]
                when (ys /= xs) $ error $ show ("Mismatch", res, xs)
                putChar '.'
        t <- myThreadId
        writeFile "Main.hs" "main = print 1"
        writeFile ".ghci" ":load Main"
        forkIO $ handle (\(e :: SomeException) -> throwTo t e) $ do
            require ["All good"]
            putStrLn "\nSuccess"
            throwTo t ExitSuccess
        f $ \msg -> unless (["Reloading..."] `isPrefixOf` msg) $
            putMVarNow ref msg

putMVarNow :: MVar a -> a -> IO ()
putMVarNow ref x = do
    b <- tryPutMVar ref x
    unless b $ error "Had a message and a new one arrived"

takeMVarDelay :: MVar a -> Double -> IO a
takeMVarDelay x i | i <= 0 = error "timed out"
takeMVarDelay x i = do
    b <- tryTakeMVar x
    case b of
        Nothing -> sleep 0.1 >> takeMVarDelay x (i-0.1)
        Just v -> return v

