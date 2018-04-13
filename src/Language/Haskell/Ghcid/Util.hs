
-- | Utility functions
module Language.Haskell.Ghcid.Util(
    ghciFlagsRequired, ghciFlagsRequiredVersioned,
    ghciFlagsUseful, ghciFlagsUsefulVersioned,
    dropPrefixRepeatedly,
    outWith, outStrLn,
    ignored,
    allGoodMessage,
    getModTime, getModTimeResolution, getShortTime
    ) where

import Control.Concurrent.Extra
import System.Time.Extra
import System.IO.Unsafe
import System.IO.Extra
import System.FilePath
import System.Info.Extra
import System.Console.ANSI
import Data.Version.Extra
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.IO.Error
import System.Directory
import Control.Exception
import Control.Monad.Extra
import Control.Applicative
import Prelude


-- | Flags that are required for ghcid to function and are supported on all GHC versions
ghciFlagsRequired :: [String]
ghciFlagsRequired =
    ["-fno-break-on-exception","-fno-break-on-error" -- see #43
    ,"-v1" -- see #110
    ]

-- | Flags that are required for ghcid to function, but are only supported on some GHC versions
ghciFlagsRequiredVersioned :: [String]
ghciFlagsRequiredVersioned =
    ["-fno-hide-source-paths" -- see #132, GHC 8.2 and above
    ]

-- | Flags that make ghcid work better and are supported on all GHC versions
ghciFlagsUseful :: [String]
ghciFlagsUseful =
    ["-ferror-spans" -- see #148
    ,"-j" -- see #153, GHC 7.8 and above, but that's all I support anyway
    ]

-- | Flags that make ghcid work better, but are only supported on some GHC versions
ghciFlagsUsefulVersioned :: [String]
ghciFlagsUsefulVersioned =
    ["-fdiagnostics-color=always" -- see #144, GHC 8.2 and above
    ]


-- | Drop a prefix from a list, no matter how many times that prefix is present
dropPrefixRepeatedly :: Eq a => [a] -> [a] -> [a]
dropPrefixRepeatedly []  s = s
dropPrefixRepeatedly pre s = maybe s (dropPrefixRepeatedly pre) $ stripPrefix pre s


{-# NOINLINE lock #-}
lock :: Lock
lock = unsafePerformIO newLock

-- | Perform some IO action with a global lock.
--   You should make sure the data required by the operation has been evaluated in advance.
outWith :: IO a -> IO a
outWith = withLock lock

outStrLn :: String -> IO ()
outStrLn xs = do
    evaluate $ length $ show xs
    outWith $ putStrLn xs


-- | Ignore all exceptions coming from an action
ignored :: IO () -> IO ()
ignored act = do
    bar <- newBarrier
    forkFinally act $ const $ signalBarrier bar ()
    waitBarrier bar

-- | The message to show when no errors have been reported
allGoodMessage :: String
allGoodMessage = setSGRCode [SetColor Foreground Dull Green] ++  "All good" ++ setSGRCode []

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
