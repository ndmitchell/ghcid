
module Util(
    outStrLn, outStr,
    withTempDirectory, withCurrentDirectory,
    sleep
    ) where

import Control.Concurrent
import System.IO.Unsafe
import System.Directory
import Control.Exception
import System.FilePath


{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

outStr :: String -> IO ()
outStr = withMVar lock . const . putStr

outStrLn :: String -> IO ()
outStrLn s = outStr $ s ++ "\n"


withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory act = do
    tdir <- fmap (</> ".ghcid") getTemporaryDirectory
    bracket_
        (createDirectoryIfMissing True tdir)
        (removeDirectoryRecursive tdir)
        $ act tdir


withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir act =
    bracket getCurrentDirectory setCurrentDirectory $ const $ do
        setCurrentDirectory dir; act


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000
