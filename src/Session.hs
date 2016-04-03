{-# LANGUAGE RecordWildCards #-}

-- | A persistent version of the Ghci session, encoding lots of semantics on top.
--   Not suitable for calling multithreaded.
module Session(
    Session, withSession, sessionStart, sessionRestart, sessionUnderlying,
    ) where

import Language.Haskell.Ghcid as G
import Language.Haskell.Ghcid.Util
import Data.IORef
import System.Time.Extra
import System.Process
import Control.Exception.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Data.List.Extra


data Session = Session
    {ghci :: IORef (Maybe Ghci)
    ,command :: IORef (Maybe String)
    }


-- | Ensure an action runs off the main thread, so can't get hit with Ctrl-C exceptions.
ctrlC :: IO a -> IO a
ctrlC = join . onceFork


-- | The function 'withGhci' expects to be run on the main thread,
--   but the inner function will not. This ensures Ctrl-C is handled
--   properly.
withSession :: (Session -> IO a) -> IO a
withSession f = do
    ghci <- newIORef Nothing
    command <- newIORef Nothing
    ctrlC (f $ Session{..}) `finally` do
        whenJustM (readIORef ghci) $ \v -> do
            writeIORef ghci Nothing
            ctrlC $ kill v


-- | Kill. Wait just long enough to ensure you've done the job, but not to see the results.
kill :: Ghci -> IO ()
kill ghci = ignore $ do
    timeout 5 $ quit ghci
    terminateProcess $ process ghci


sessionStart :: Session -> String -> IO ([Load], [FilePath])
sessionStart Session{..} cmd = do
    writeIORef command $ Just cmd
    val <- readIORef ghci
    whenJust val $ void . forkIO . kill
    writeIORef ghci Nothing
    (v, load) <- startGhci cmd Nothing (const outStrLn)
    writeIORef ghci $ Just v
    return (load, nubOrd $ map loadFile load)

sessionRestart :: Session -> IO ([Load], [FilePath])
sessionRestart session@Session{..} = do
    cmd <- readIORef command
    maybe (fail "Restart called before start") (sessionStart session) cmd


sessionUnderlying :: Session -> IO Ghci
sessionUnderlying Session{..} = do
    v <- readIORef ghci
    maybe (fail "Underlying called before start") return v
