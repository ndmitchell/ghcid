-- | Utility functions
-- Copyright Neil Mitchell 2014.
module Language.Haskell.Ghcid.Util
  ( dropPrefixRepeatedly
  , chunksOfWord
  , outWith
  , outStrLn
  , outStr
  , allGoodMessage
  ) where

import Control.Concurrent.Extra
import System.IO.Unsafe
import Data.List.Extra
import Data.Char


-- | Drop a prefix from a list, no matter how many times that prefix is present
dropPrefixRepeatedly :: Eq a => [a] -> [a] -> [a]
dropPrefixRepeatedly []  s = s
dropPrefixRepeatedly pre s = maybe s (dropPrefixRepeatedly pre) $ stripPrefix pre s


{-# NOINLINE lock #-}
lock :: Lock
lock = unsafePerformIO newLock

outWith :: IO a -> IO a
outWith = withLock lock

outStr :: String -> IO ()
outStr = outWith . putStr

outStrLn :: String -> IO ()
outStrLn s = outStr $ s ++ "\n"


-- | The message to show when no errors have been reported
allGoodMessage :: String      
allGoodMessage = "All good"

-- | Like chunksOf, but deal with words up to some gap.
--   Flows onto a subsequent line if less than N characters end up being empty.
chunksOfWord :: Int -> Int -> String -> [String]
chunksOfWord mx gap = repeatedly $ \x ->
    let (a,b) = splitAt mx x in
    if null b then (a, []) else
        let (a1,a2) = breakEnd isSpace a in
        if length a2 <= gap then (a1, a2 ++ b) else (a, dropWhile isSpace b)
