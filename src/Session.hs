{-# LANGUAGE RecordWildCards #-}

-- | A persistent version of the Ghci session, encoding lots of semantics on top.
--   Not suitable for calling multithreaded.
module Session(
    Session, withSession,
    sessionStart, sessionRestart, sessionReload,
    sessionExecAsync,
    ) where

import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Util
import Data.IORef
import System.Time.Extra
import System.Process
import Control.Exception.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Data.Maybe
import Data.List.Extra
import Control.Applicative
import Prelude


data Session = Session
    {ghci :: IORef (Maybe Ghci) -- ^ The Ghci session, or Nothing if there is none
    ,command :: IORef (Maybe String) -- ^ The last command passed to sessionStart
    ,warnings :: IORef [Load] -- ^ The warnings from the last load
    ,running :: Var Bool -- ^ Am I actively running an async command
    }


-- | Ensure an action runs off the main thread, so can't get hit with Ctrl-C exceptions.
ctrlC :: IO a -> IO a
ctrlC = join . onceFork


-- | The function 'withSession' expects to be run on the main thread,
--   but the inner function will not. This ensures Ctrl-C is handled
--   properly and any spawned Ghci processes will be aborted.
withSession :: (Session -> IO a) -> IO a
withSession f = do
    ghci <- newIORef Nothing
    command <- newIORef Nothing
    warnings <- newIORef []
    running <- newVar False
    ctrlC (f $ Session{..}) `finally` do
        modifyVar_ running $ const $ return False
        whenJustM (readIORef ghci) $ \v -> do
            writeIORef ghci Nothing
            ctrlC $ kill v


-- | Kill. Wait just long enough to ensure you've done the job, but not to see the results.
kill :: Ghci -> IO ()
kill ghci = ignore $ do
    timeout 5 $ quit ghci
    terminateProcess $ process ghci


-- | Spawn a new Ghci process at a given command line. Returns the load messages, plus
--   the list of files that were observed (both those loaded and those that failed to load).
sessionStart :: Session -> String -> IO ([Load], [FilePath])
sessionStart Session{..} cmd = do
    modifyVar_ running $ const $ return False
    writeIORef command $ Just cmd
    val <- readIORef ghci
    whenJust val $ void . forkIO . kill
    writeIORef ghci Nothing
    outStrLn $ "Loading " ++ cmd ++ " ..."
    (v, messages) <- startGhci cmd Nothing $ const outStrLn
    writeIORef ghci $ Just v
    messages <- return $ mapMaybe tidyMessage messages
    writeIORef warnings [m | m@Message{..} <- messages, loadSeverity == Warning]
    return (messages, nubOrd $ map loadFile messages)


-- | Call 'sessionStart' at the previous command.
sessionRestart :: Session -> IO ([Load], [FilePath])
sessionRestart session@Session{..} = do
    Just cmd <- readIORef command
    sessionStart session cmd


-- | Reload, returning the same information as 'sessionStart'. In particular, any
--   information that GHCi doesn't repeat (warnings from loaded modules) will be
--   added back in.
sessionReload :: Session -> IO ([Load], [FilePath])
sessionReload session@Session{..} = do
    -- kill anything async, set stuck if you didn't succeed
    old <- modifyVar running $ \b -> return (False, b)
    stuck <- if not old then return False else do
        Just ghci <- readIORef ghci
        fmap isNothing $ timeout 5 $ interrupt ghci

    if stuck then sessionRestart session else do
        -- actually reload
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


-- | Run an exec operation asynchronously. Should not be a @:reload@ or similar.
--   Will be automatically aborted if it takes too long. Only fires done if not aborted.
--   Argument to done is the final stderr line.
sessionExecAsync :: Session -> String -> (String -> IO ()) -> IO ()
sessionExecAsync Session{..} cmd done = do
    Just ghci <- readIORef ghci
    stderr <- newIORef ""
    modifyVar_ running $ const $ return True
    void $ forkIO $ do
        execStream ghci cmd $ \strm msg ->
            when (msg /= "*** Exception: ExitSuccess") $ do
                when (strm == Stderr) $ writeIORef stderr msg
                outStrLn msg
        old <- modifyVar running $ \b -> return (False, b)
        -- don't fire Done if someone interrupted us
        stderr <- readIORef stderr
        when old $ done stderr


-- | Ignore entirely pointless messages and remove unnecessary lines.
tidyMessage :: Load -> Maybe Load
tidyMessage Message{loadSeverity=Warning, loadMessage=[_,x]}
    | x == "    -O conflicts with --interactive; -O ignored." = Nothing
tidyMessage m@Message{..}
    = Just m{loadMessage = filter (\x -> not $ any (`isPrefixOf` x) bad) loadMessage}
    where bad = ["      except perhaps to import instances from"
                ,"    To import instances alone, use: import "]
tidyMessage x = Just x
