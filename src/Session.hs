{-# LANGUAGE RecordWildCards #-}

-- | A persistent version of the Ghci session, encoding lots of semantics on top.
--   Not suitable for calling multithreaded.
module Session(
    Session, enableEval, withSession,
    sessionStart, sessionReload,
    sessionExecAsync,
    ) where

import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Escape
import Language.Haskell.Ghcid.Util
import Language.Haskell.Ghcid.Types
import Data.IORef
import System.Console.ANSI
import System.Time.Extra
import System.Process
import System.FilePath
import Control.Exception.Extra
import Control.Concurrent.Extra
import Control.Monad.Extra
import Data.Maybe
import Data.List.Extra
import Control.Applicative
import Prelude
import System.IO.Extra


data Session = Session
    {ghci :: IORef (Maybe Ghci) -- ^ The Ghci session, or Nothing if there is none
    ,command :: IORef (Maybe (String, [String])) -- ^ The last command passed to sessionStart, setup operations
    ,warnings :: IORef [Load] -- ^ The warnings from the last load
    ,curdir :: IORef FilePath -- ^ The current working directory
    ,running :: Var Bool -- ^ Am I actively running an async command
    ,withThread :: ThreadId -- ^ Thread that called withSession
    ,allowEval :: Bool  -- ^ Is the allow-eval flag set?
    }

enableEval :: Session -> Session
enableEval s = s { allowEval = True }


debugShutdown x = when False $ print ("DEBUG SHUTDOWN", x)

-- | The function 'withSession' expects to be run on the main thread,
--   but the inner function will not. This ensures Ctrl-C is handled
--   properly and any spawned Ghci processes will be aborted.
withSession :: (Session -> IO a) -> IO a
withSession f = do
    ghci <- newIORef Nothing
    command <- newIORef Nothing
    warnings <- newIORef []
    curdir <- newIORef "."
    running <- newVar False
    debugShutdown "Starting session"
    withThread <- myThreadId
    let allowEval = False
    f Session{..} `finally` do
        debugShutdown "Start finally"
        modifyVar_ running $ const $ pure False
        whenJustM (readIORef ghci) $ \v -> do
            writeIORef ghci Nothing
            debugShutdown "Calling kill"
            kill v
        debugShutdown "Finish finally"


-- | Kill. Wait just long enough to ensure you've done the job, but not to see the results.
kill :: Ghci -> IO ()
kill ghci = ignored $ do
    timeout 5 $ do
        debugShutdown "Before quit"
        ignored $ quit ghci
        debugShutdown "After quit"
    debugShutdown "Before killProcessGroup"
    ignored $ killProcessGroup $ process ghci
    debugShutdown "After killProcessGroup"
    -- Ctrl-C after a tests keeps the cursor hidden,
    -- `setSGR []`didn't seem to be enough
    -- See: https://github.com/ndmitchell/ghcid/issues/254
    showCursor

loadedModules :: FilePath -> [Load] -> [FilePath]
loadedModules dir = nubOrd . map (loadFile . qualify dir) . filter predicate
    where
      predicate Message{loadFile = loadFile} = loadFile /= "<unknown>"
      predicate Loading{loadFile = loadFile} = loadFile /= "<unknown>"
      predicate _ = False

qualify :: FilePath -> Load -> Load
qualify dir message = message{loadFile = dir </> loadFile message}

-- | Spawn a new Ghci process at a given command line. Returns the load messages, plus
--   the list of files that were observed (both those loaded and those that failed to load).
sessionStart :: Session -> String -> [String] -> IO ([Load], [FilePath])
sessionStart Session{..} cmd setup = do
    modifyVar_ running $ const $ pure False
    writeIORef command $ Just (cmd, setup)

    -- cleanup any old instances
    whenJustM (readIORef ghci) $ \v -> do
        writeIORef ghci Nothing
        void $ forkIO $ kill v

    -- start the new
    outStrLn $ "Loading " ++ cmd ++ " ..."
    (v, messages) <- mask $ \unmask -> do
        (v, messages) <- unmask $ startGhci cmd Nothing $ const outStrLn
        writeIORef ghci $ Just v
        pure (v, messages)

    -- do whatever preparation was requested
    exec v $ unlines setup

    -- deal with current directory
    (dir, _) <- showPaths v
    writeIORef curdir dir
    messages <- pure $ map (qualify dir) messages

    let loaded = loadedModules dir messages
    evals <- performEvals v allowEval loaded

    -- install a handler
    forkIO $ do
        code <- waitForProcess $ process v
        whenJustM (readIORef ghci) $ \ghci ->
            when (ghci == v) $ do
                sleep 0.3 -- give anyone reading from the stream a chance to throw first
                throwTo withThread $ ErrorCall $ "Command \"" ++ cmd ++ "\" exited unexpectedly with " ++ show code

    -- handle what the process returned
    messages <- pure $ mapMaybe tidyMessage messages
    writeIORef warnings $ getWarnings messages
    pure (messages ++ evals, loaded)


getWarnings :: [Load] -> [Load]
getWarnings messages = [m | m@Message{..} <- messages, loadSeverity == Warning]


-- | Call 'sessionStart' at the previous command.
sessionRestart :: Session -> IO ([Load], [FilePath])
sessionRestart session@Session{..} = do
    Just (cmd, setup) <- readIORef command
    sessionStart session cmd setup


performEvals :: Ghci -> Bool -> [FilePath] -> IO [Load]
performEvals _ False _ = pure []
performEvals ghci True reloaded = do
    cmds <- mapM getCommands reloaded
    fmap join $ forM cmds $ \(file, cmds') ->
        forM cmds' $ \(num, cmd) -> do
            ref <- newIORef []
            execStream ghci cmd $ \_ resp -> modifyIORef ref (resp :)
            resp <- unlines . reverse <$> readIORef ref
            pure $ Eval $ EvalResult file (num, 1) cmd resp


getCommands :: FilePath -> IO (FilePath, [(Int, String)])
getCommands fp = do
    ls <- readFileUTF8' fp
    pure (fp, splitCommands $ zipFrom 1 $ lines ls)

splitCommands :: [(Int, String)] -> [(Int, String)]
splitCommands [] = []
splitCommands ((num, line) : ls)
    | isCommand line =
          let (cmds, xs) = span (isCommand . snd) ls
           in (num, unwords $ fmap (drop $ length commandPrefix) $ line : fmap snd cmds) : splitCommands xs
    | isMultilineCommandPrefix line =
          let (cmds, xs) = break (isMultilineCommandSuffix . snd) ls
           in (num, unlines (wrapGhciMultiline (fmap snd cmds))) : splitCommands (drop1 xs)
    | otherwise = splitCommands ls

isCommand :: String -> Bool
isCommand = isPrefixOf commandPrefix

commandPrefix :: String
commandPrefix = "-- $> "

isMultilineCommandPrefix :: String -> Bool
isMultilineCommandPrefix = (==) multilineCommandPrefix

multilineCommandPrefix :: String
multilineCommandPrefix = "{- $>"

isMultilineCommandSuffix :: String -> Bool
isMultilineCommandSuffix = (==) multilineCommandSuffix

multilineCommandSuffix :: String
multilineCommandSuffix = "<$ -}"

wrapGhciMultiline :: [String] -> [String]
wrapGhciMultiline xs = [":{"] ++ xs ++ [":}"]

-- | Reload, returning the same information as 'sessionStart'. In particular, any
--   information that GHCi doesn't repeat (warnings from loaded modules) will be
--   added back in.
sessionReload :: Session -> IO ([Load], [FilePath], [FilePath])
sessionReload session@Session{..} = do
    -- kill anything async, set stuck if you didn't succeed
    old <- modifyVar running $ \b -> pure (False, b)
    stuck <- if not old then pure False else do
        Just ghci <- readIORef ghci
        fmap isNothing $ timeout 5 $ interrupt ghci

    if stuck
      then (\(messages,loaded) -> (messages,loaded,loaded)) <$> sessionRestart session
      else do
        -- actually reload
        Just ghci <- readIORef ghci
        dir <- readIORef curdir
        messages <- mapMaybe tidyMessage <$> reload ghci
        loaded <- map ((dir </>) . snd) <$> showModules ghci
        let reloaded = loadedModules dir messages
        warn <- readIORef warnings
        evals <- performEvals ghci allowEval reloaded

        -- only keep old warnings from files that are still loaded, but did not reload
        let validWarn w = loadFile w `elem` loaded && loadFile w `notElem` reloaded
        -- newest warnings always go first, so the file you hit save on most recently has warnings first
        messages <- pure $ messages ++ filter validWarn warn

        writeIORef warnings $ getWarnings messages
        pure (messages ++ evals, nubOrd (loaded ++ reloaded), reloaded)


-- | Run an exec operation asynchronously. Should not be a @:reload@ or similar.
--   Will be automatically aborted if it takes too long. Only fires done if not aborted.
--   Argument to done is the final stderr line.
sessionExecAsync :: Session -> String -> (String -> IO ()) -> IO ()
sessionExecAsync Session{..} cmd done = do
    Just ghci <- readIORef ghci
    stderr <- newIORef ""
    modifyVar_ running $ const $ pure True
    caller <- myThreadId
    void $ flip forkFinally (either (throwTo caller) (const $ pure ())) $ do
        execStream ghci cmd $ \strm msg ->
            when (msg /= "*** Exception: ExitSuccess") $ do
                when (strm == Stderr) $ writeIORef stderr msg
                outStrLn msg
        old <- modifyVar running $ \b -> pure (False, b)
        -- don't fire Done if someone interrupted us
        stderr <- readIORef stderr
        when old $ done stderr


-- | Ignore entirely pointless messages and remove unnecessary lines.
tidyMessage :: Load -> Maybe Load
tidyMessage Message{loadSeverity=Warning, loadMessage=[_,x]}
    | unescape x == "    -O conflicts with --interactive; -O ignored." = Nothing
tidyMessage m@Message{..}
    = Just m{loadMessage = filter (\x -> not $ any (`isPrefixOf` unescape x) bad) loadMessage}
    where bad = ["      except perhaps to import instances from"
                ,"    To import instances alone, use: import "]
tidyMessage x = Just x
