-- | Utility functions
-- Copyright Neil Mitchell 2014.
module Language.Haskell.Ghcid.Util
  ( dropPrefixRepeatedly
  , outStrLn
  , outStr
  ) where

import Control.Concurrent.Extra
import System.IO.Unsafe
import Data.List

-- | Drop a prefix from a list, no matter how many times that prefix is present
dropPrefixRepeatedly :: Eq a => [a] -> [a] -> [a]
dropPrefixRepeatedly []  s = s
dropPrefixRepeatedly pre s = maybe s (dropPrefixRepeatedly pre) $ stripPrefix pre s


{-# NOINLINE lock #-}
lock :: Lock
lock = unsafePerformIO newLock

outStr :: String -> IO ()
outStr = withLock lock . putStr

outStrLn :: String -> IO ()
outStrLn s = outStr $ s ++ "\n"
