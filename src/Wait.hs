{-# LANGUAGE ScopedTypeVariables #-}

-- | Use 'withWaiterPoll' or 'withWaiterNotify' to create a 'Waiter' object,
--   then access it (single-threaded) by using 'waitFiles'.
module Wait(Waiter, withWaiterPoll, withWaiterNotify, waitFiles) where

import Control.Concurrent.Extra
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Extra
import Data.List.Extra
import System.FilePath
import Control.Exception.Extra
import System.Directory.Extra
import Data.Time.Clock
import Data.String
import Data.Maybe
import System.Console.CmdArgs
import System.Time.Extra
import System.FSNotify
import Language.Haskell.Ghcid.Util


data Waiter = WaiterPoll Seconds
            | WaiterNotify WatchManager (MVar ()) (Var (Map.Map FilePath StopListening))

withWaiterPoll :: Seconds -> (Waiter -> IO a) -> IO a
withWaiterPoll x f = f $ WaiterPoll x

withWaiterNotify :: (Waiter -> IO a) -> IO a
withWaiterNotify f = withManagerConf defaultConfig{confDebounce=NoDebounce} $ \manager -> do
    mvar <- newEmptyMVar
    var <- newVar Map.empty
    f $ WaiterNotify manager mvar var


-- | Given the pattern:
--
-- > wait <- waitFiles waiter
-- > ...
-- > wait ["File1.hs","File2.hs"]
--
--   This continues as soon as either @File1.hs@ or @File2.hs@ changes,
--   starting from when 'waitFiles' was initially called.
--
--   Returns a message about why you are continuing (usually a file name).
waitFiles :: Waiter -> IO ([FilePath] -> IO [String])
waitFiles waiter = do
    base <- getCurrentTime
    return $ \files -> handle (\(e :: IOError) -> do sleep 1.0; return ["Error when waiting, if this happens repeatedly, raise a ghcid bug.",show e]) $ do
        whenLoud $ outStrLn $ "%WAITING: " ++ unwords files
        files <- fmap concat $ forM files $ \file ->
            ifM (doesDirectoryExist file) (listFilesInside (return . not . isPrefixOf "." . takeFileName) file) (return [file])
        case waiter of
            WaiterPoll t -> sleep $ max 0 $ t - 0.1 -- subtract the initial 0.1 sleep from above
            WaiterNotify manager kick mp -> do
                dirs <- fmap Set.fromList $ mapM canonicalizePathSafe $ nubOrd $ map takeDirectory files
                modifyVar_ mp $ \mp -> do
                    let (keep,del) = Map.partitionWithKey (\k v -> k `Set.member` dirs) mp
                    sequence_ $ Map.elems del
                    new <- forM (Set.toList $ dirs `Set.difference` Map.keysSet keep) $ \dir -> do
                        can <- watchDir manager (fromString dir) (const True) $ \event -> do
                            whenLoud $ outStrLn $ "%NOTIFY: " ++ show event
                            void $ tryPutMVar kick ()
                        return (dir, can)
                    let mp2 = keep `Map.union` Map.fromList new
                    whenLoud $ outStrLn $ "%WAITING: " ++ unwords (Map.keys mp2)
                    return mp2
                void $ tryTakeMVar kick
        new <- mapM getModTime files
        case [x | (x,Just t) <- zip files new, t > base] of
            [] -> recheck files new
            xs -> return xs
    where
        recheck files old = do
            sleep 0.1
            case waiter of
                WaiterPoll _ -> return ()
                WaiterNotify _ kick _ -> do
                    takeMVar kick
                    whenLoud $ outStrLn "%WAITING: Notify signaled"
            new <- mapM getModTime files
            case [x | (x,t1,t2) <- zip3 files old new, t1 /= t2] of
                [] -> recheck files new
                xs -> do
                    when (or $ zipWith (\o n -> isJust o && isNothing n) old new) $ do
                        -- if someone is deleting a needed file, give them some space to put the file back
                        -- typically caused by VIM
                        whenLoud $ outStrLn "%WAITING: Extra wait due to file removal"
                        sleep 1
                    return xs


canonicalizePathSafe :: FilePath -> IO FilePath
canonicalizePathSafe x = canonicalizePath x `catch_` const (return x)
