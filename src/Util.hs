
module Util(
    outStrLn, outStr,
    sleep
    ) where

import Control.Concurrent
import System.IO.Unsafe


{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

outStr :: String -> IO ()
outStr = withMVar lock . const . putStr

outStrLn :: String -> IO ()
outStrLn s = outStr $ s ++ "\n"


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000
