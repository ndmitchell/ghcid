
-- | A persistent version of the Ghci session, encoding lots of semantics on top.
module Session(
    Session, withSession, start,
    ) where

import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Util
import Data.IORef
import System.Time.Extra
import System.Process
import Control.Exception.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra


newtype Session = Session (IORef (Maybe Ghci))


-- | Ensure an action runs off the main thread, so can't get hit with Ctrl-C exceptions.
ctrlC :: IO a -> IO a
ctrlC = join . onceFork


-- | The function 'withGhci' expects to be run on the main thread,
--   but the inner function will not. This ensures Ctrl-C is handled
--   properly.
withSession :: (Session -> IO a) -> IO a
withSession f = do
    ref <- newIORef Nothing
    ctrlC (f $ Session ref) `finally` do
        whenJustM (readIORef ref) $ \ghci -> do
            writeIORef ref Nothing
            ctrlC $ kill ghci


-- | Kill. Wait just long enough to ensure you've done the job, but not to see the results.
kill :: Ghci -> IO ()
kill ghci = ignore $ do
    timeout 5 $ quit ghci
    terminateProcess $ process ghci


start :: Session -> String -> IO [Load]
start (Session ref) command = do
    val <- readIORef ref
    whenJust val $ void . forkIO . kill
    writeIORef ref Nothing
    (ghci, load) <- startGhci command Nothing (const outStrLn)
    writeIORef ref $ Just ghci
    return load
