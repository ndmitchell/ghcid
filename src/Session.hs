{-# LANGUAGE RecordWildCards #-}

-- | A persistent version of the Ghci session, encoding lots of semantics on top.
--   Not suitable for calling multithreaded.
module Session(
    Session, withSession, sessionUnderlying,
    sessionStart, sessionRestart, sessionReload
    ) where

import Language.Haskell.Ghcid as G
import Language.Haskell.Ghcid.Util
import Data.IORef
import System.Time.Extra
import System.Process
import Control.Exception.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Data.Maybe
import Data.List.Extra


data Session = Session
    {ghci :: IORef (Maybe Ghci) -- ^ The Ghci session, or Nothing if there is none
    ,command :: IORef (Maybe String) -- ^ The last command passed to sessionStart
    ,warnings :: IORef [Load] -- ^ The warnings from the last load
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
    warnings <- newIORef []
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
    outStrLn $ "Loading " ++ cmd ++ " ..."
    (v, messages) <- startGhci cmd Nothing (const outStrLn)
    writeIORef ghci $ Just v
    messages <- return $ mapMaybe tidyMessage messages
    writeIORef warnings [m | m@Message{..} <- messages, loadSeverity == Warning]
    return (messages, nubOrd $ map loadFile messages)

sessionRestart :: Session -> IO ([Load], [FilePath])
sessionRestart session@Session{..} = do
    Just cmd <- readIORef command
    sessionStart session cmd


sessionReload :: Session -> IO ([Load], [FilePath])
sessionReload Session{..} = do
    Just ghci <- readIORef ghci
    messages <- mapMaybe tidyMessage <$> reload ghci
    loaded <- map snd <$> showModules ghci
    let reloaded = nubOrd $ filter (/= "") $ map loadFile messages
    warn <- readIORef warnings

   -- only keep old warnings from files that are still loaded, but did not reload
    let validWarn w = loadFile w `elem` loaded && loadFile w `notElem` reloaded
    -- newest warnings always go first, so the file you hit save on most recently has warnings first
    messages <- return $ messages ++ filter validWarn warn

    writeIORef warnings [m | m@Message{..} <- messages, loadSeverity == Warning]
    return (messages, nubOrd $ loaded ++ reloaded)


sessionUnderlying :: Session -> IO Ghci
sessionUnderlying Session{..} = do
    Just ghci <- readIORef ghci
    return ghci


-- | Ignore entirely pointless messages and remove unnecessary lines.
tidyMessage :: Load -> Maybe Load
tidyMessage Message{loadSeverity=Warning, loadMessage=[_,x]}
    | x == "    -O conflicts with --interactive; -O ignored." = Nothing
tidyMessage m@Message{..}
    = Just m{loadMessage = filter (\x -> not $ any (`isPrefixOf` x) bad) loadMessage}
    where bad = ["      except perhaps to import instances from"
                ,"    To import instances alone, use: import "]
tidyMessage x = Just x
