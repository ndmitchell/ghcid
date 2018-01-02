
-- | Utility functions
module Language.Haskell.Ghcid.Util(
    dropPrefixRepeatedly,
    chunksOfWord,
    outWith, outStrLn,
    allGoodMessage,
    getModTime, getModTimeResolution, getShortTime
    ) where

import Control.Concurrent.Extra
import System.Time.Extra
import System.IO.Unsafe
import System.IO.Extra
import System.FilePath
import System.Info.Extra
import Data.Version.Extra
import Data.List.Extra
import Data.Char
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.IO.Error
import System.Directory
import Control.Exception
import Control.Monad.Extra
import Control.Applicative
import Prelude


-- | Drop a prefix from a list, no matter how many times that prefix is present
dropPrefixRepeatedly :: Eq a => [a] -> [a] -> [a]
dropPrefixRepeatedly []  s = s
dropPrefixRepeatedly pre s = maybe s (dropPrefixRepeatedly pre) $ stripPrefix pre s


{-# NOINLINE lock #-}
lock :: Lock
lock = unsafePerformIO newLock

outWith :: IO a -> IO a
outWith = withLock lock

outStrLn :: String -> IO ()
outStrLn = outWith . putStrLn


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

-- | Given a 'FilePath' return either 'Nothing' (file does not exist) or 'Just' (the modification time)
getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime file = handleJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (\_ -> return Nothing)
    (Just <$> getModificationTime file)


-- | Get the current time in the current timezone in HH:MM:SS format
getShortTime :: IO String
getShortTime = formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime


-- | Get the smallest difference that can be reported by two modification times
getModTimeResolution :: IO Seconds
getModTimeResolution = return getModTimeResolutionCache

{-# NOINLINE getModTimeResolutionCache #-}
-- Cache the result so only computed once per run
getModTimeResolutionCache :: Seconds
getModTimeResolutionCache = unsafePerformIO $ withTempDir $ \dir -> do
    let file = dir </> "calibrate.txt"

    -- with 10 measurements can get a bit slow, see Shake issue tracker #451
    -- if it rounds to a second then 1st will be a fraction, but 2nd will be full second
    mtime <- fmap maximum $ forM [1..3] $ \i -> fmap fst $ duration $ do
        writeFile file $ show i
        t1 <- getModificationTime file
        flip loopM 0 $ \j -> do
            writeFile file $ show (i,j)
            t2 <- getModificationTime file
            return $ if t1 == t2 then Left $ j+1 else Right ()

    -- GHC 7.6 and below only have 1 sec resolution timestamps
    mtime <- return $ if compilerVersion < makeVersion [7,8] then max mtime 1 else mtime

    putStrLn $ "Longest file modification time lag was " ++ show (ceiling (mtime * 1000)) ++ "ms"
    -- add a little bit of safety, but if it's really quick, don't make it that much slower
    return $ mtime + min 0.1 mtime
