{-# LANGUAGE RecordWildCards #-}

-- | Library for spawning and working with Ghci sessions.
module Language.Haskell.Ghcid(
    Ghci, GhciError(..), Stream(..),
    Load(..), Severity(..),
    startGhci, startGhciProcess, stopGhci, interrupt, process,
    execStream, showModules, showPaths, reload, exec, quit
    ) where

import System.IO
import System.IO.Error
import System.Process
import System.Time.Extra
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Function
import Data.List.Extra
import Data.Maybe
import Data.IORef
import Control.Applicative
import Data.Unique

import System.Console.CmdArgs.Verbosity

import Language.Haskell.Ghcid.Parser
import Language.Haskell.Ghcid.Types as T
import Language.Haskell.Ghcid.Util
import Prelude


-- | A GHCi session. Created with 'startGhci', closed with 'stopGhci'.
--
--   The interactions with a 'Ghci' session must all occur single-threaded,
--   or an error will be raised. The only exception is 'interrupt', which aborts
--   a running computation, or does nothing if no computation is running.
data Ghci = Ghci
    {ghciProcess :: ProcessHandle
    ,ghciInterrupt :: IO ()
    ,ghciExec :: String -> (Stream -> String -> IO ()) -> IO ()
    ,ghciUnique :: Unique
    }

instance Eq Ghci where
    a == b = ghciUnique a == ghciUnique b


withCreateProc proc f = do
    let undo (_, _, _, proc) = do ignored $ terminateProcess proc
    bracketOnError (createProcess proc) undo $ \(a,b,c,d) -> f a b c d

-- | Start GHCi by running the described process, returning  the result of the initial loading.
--   If you do not call 'stopGhci' then the underlying process may be leaked.
--   The callback will be given the messages produced while loading, useful if invoking something like "cabal repl"
--   which might compile dependent packages before really loading.
--
--   To create a 'CreateProcess' use the functions in "System.Process", particularly
--   'System.Process.shell' and 'System.Process.proc'.
--
--   @since 0.6.11
startGhciProcess :: CreateProcess -> (Stream -> String -> IO ()) -> IO (Ghci, [Load])
startGhciProcess process echo0 = do
    let proc = process{std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, create_group=True}
    withCreateProc proc $ \(Just inp) (Just out) (Just err) ghciProcess -> do

        hSetBuffering out LineBuffering
        hSetBuffering err LineBuffering
        hSetBuffering inp LineBuffering
        let writeInp x = do
                whenLoud $ outStrLn $ "%STDIN: " ++ x
                hPutStrLn inp x

        -- Some programs (e.g. stack) might use stdin before starting ghci (see #57)
        -- Send them an empty line
        hPutStrLn inp ""

        -- I'd like the GHCi prompt to go away, but that's not possible, so I set it to a special
        -- string and filter that out.
        let ghcid_prefix = "#~GHCID-START~#"
        let removePrefix = dropPrefixRepeatedly ghcid_prefix

        -- At various points I need to ensure everything the user is waiting for has completed
        -- So I send messages on stdout/stderr and wait for them to arrive
        syncCount <- newVar 0
        let syncReplay = do
                i <- readVar syncCount
                -- useful to avoid overloaded strings by showing the ['a','b','c'] form, see #109
                let showStr xs = "[" ++ intercalate "," (map show xs) ++ "]"
                let msg = "#~GHCID-FINISH-" ++ show i ++ "~#"
                writeInp $ "INTERNAL_GHCID.putStrLn " ++ showStr msg ++ "\n" ++
                        "INTERNAL_GHCID.hPutStrLn INTERNAL_GHCID.stderr " ++ showStr msg
                return $ isInfixOf msg
        let syncFresh = do
                modifyVar_ syncCount $ return . succ
                syncReplay

        -- Consume from a stream until EOF (return Nothing) or some predicate returns Just
        let consume :: Stream -> (String -> IO (Maybe a)) -> IO (Maybe a)
            consume name finish = do
                let h = if name == Stdout then out else err
                fix $ \rec -> do
                    el <- tryBool isEOFError $ hGetLine h
                    case el of
                        Left _ -> return Nothing
                        Right l -> do
                            whenLoud $ outStrLn $ "%" ++ upper (show name) ++ ": " ++ l
                            res <- finish $ removePrefix l
                            case res of
                                Nothing -> rec
                                Just a -> return $ Just a

        let consume2 :: String -> (Stream -> String -> IO (Maybe a)) -> IO (a,a)
            consume2 msg finish = do
                -- fetch the operations in different threads as hGetLine may block
                -- and can't be aborted by async exceptions, see #154
                res1 <- onceFork $ consume Stdout (finish Stdout)
                res2 <- onceFork $ consume Stderr (finish Stderr)
                res1 <- res1
                res2 <- res2
                case liftM2 (,) res1 res2 of
                    Nothing -> case cmdspec process of
                        ShellCommand cmd -> throwIO $ UnexpectedExit cmd msg
                        RawCommand exe args -> throwIO $ UnexpectedExit (unwords (exe:args)) msg
                    Just v -> return v

        -- held while interrupting, and briefly held when starting an exec
        -- ensures exec values queue up behind an ongoing interrupt and no two interrupts run at once
        isInterrupting <- newLock

        -- is anyone running running an exec statement, ensure only one person talks to ghci at a time
        isRunning <- newLock

        let ghciExec command echo = do
                withLock isInterrupting $ return ()
                res <- withLockTry isRunning $ do
                    writeInp command
                    stop <- syncFresh
                    void $ consume2 command $ \strm s ->
                        if stop s then return $ Just () else do echo strm s; return Nothing
                when (isNothing res) $
                    fail "Ghcid.exec, computation is already running, must be used single-threaded"

        let ghciInterrupt = withLock isInterrupting $
                whenM (fmap isNothing $ withLockTry isRunning $ return ()) $ do
                    whenLoud $ outStrLn "%INTERRUPT"
                    interruptProcessGroupOf ghciProcess
                    -- let the person running ghciExec finish, since their sync messages
                    -- may have been the ones that got interrupted
                    syncReplay
                    -- now wait for the person doing ghciExec to have actually left the lock
                    withLock isRunning $ return ()
                    -- there may have been two syncs sent, so now do a fresh sync to clear everything
                    stop <- syncFresh
                    void $ consume2 "Interrupt" $ \_ s -> return $ if stop s then Just () else Nothing

        ghciUnique <- newUnique
        let ghci = Ghci{..}

        -- Now wait for 'GHCi, version' to appear before sending anything real, required for #57
        stdout <- newIORef []
        stderr <- newIORef []
        sync <- newIORef $ const False
        consume2 "" $ \strm s -> do
            stop <- readIORef sync
            if stop s then
                return $ Just ()
            else do
                -- there may be some initial prompts on stdout before I set the prompt properly
                s <- return $ maybe s (removePrefix . snd) $ stripInfix ghcid_prefix s
                whenLoud $ outStrLn $ "%STDOUT2: " ++ s
                modifyIORef (if strm == Stdout then stdout else stderr) (s:)
                when ("GHCi, version " `isPrefixOf` s) $ do
                    -- the thing before me may have done its own Haskell compiling
                    writeIORef stdout []
                    writeIORef stderr []
                    writeInp "import qualified System.IO as INTERNAL_GHCID"
                    writeInp $ ":set prompt " ++ ghcid_prefix

                    -- failure isn't harmful, so do them one-by-one
                    forM_ (ghciFlagsRequired ++ ghciFlagsRequiredVersioned) $ \flag ->
                        writeInp $ ":set " ++ flag
                    writeIORef sync =<< syncFresh
                echo0 strm s
                return Nothing
        r1 <- parseLoad . reverse <$> ((++) <$> readIORef stderr <*> readIORef stdout)
        -- see #132, if hide-source-paths was turned on the modules didn't get printed out properly
        -- so try a showModules to capture the information again
        r2 <- if any isLoading r1 then return [] else map (uncurry Loading) <$> showModules ghci
        execStream ghci "" echo0
        return (ghci, r1 ++ r2)


-- | Start GHCi by running the given shell command, a helper around 'startGhciProcess'.
startGhci
    :: String -- ^ Shell command
    -> Maybe FilePath -- ^ Working directory
    -> (Stream -> String -> IO ()) -- ^ Output callback
    -> IO (Ghci, [Load])
startGhci cmd directory = startGhciProcess (shell cmd){cwd=directory}


-- | Execute a command, calling a callback on each response.
--   The callback will be called single threaded.
execStream :: Ghci -> String -> (Stream -> String -> IO ()) -> IO ()
execStream = ghciExec

-- | Interrupt Ghci, stopping the current computation (if any),
--   but leaving the process open to new input.
interrupt :: Ghci -> IO ()
interrupt = ghciInterrupt

-- | Obtain the progress handle behind a GHCi instance.
process :: Ghci -> ProcessHandle
process = ghciProcess


---------------------------------------------------------------------
-- SUGAR HELPERS

-- | Execute a command, calling a callback on each response.
--   The callback will be called single threaded.
execBuffer :: Ghci -> String -> (Stream -> String -> IO ()) -> IO [String]
execBuffer ghci cmd echo = do
    stdout <- newIORef []
    stderr <- newIORef []
    execStream ghci cmd $ \strm s -> do
        modifyIORef (if strm == Stdout then stdout else stderr) (s:)
        echo strm s
    reverse <$> ((++) <$> readIORef stderr <*> readIORef stdout)

-- | Send a command, get lines of result. Must be called single-threaded.
exec :: Ghci -> String -> IO [String]
exec ghci cmd = execBuffer ghci cmd $ \_ _ -> return ()

-- | List the modules currently loaded, with module name and source file.
showModules :: Ghci -> IO [(String,FilePath)]
showModules ghci = parseShowModules <$> exec ghci ":show modules"

-- | Return the current working directory, and a list of module import paths
showPaths :: Ghci -> IO (FilePath, [FilePath])
showPaths ghci = parseShowPaths <$> exec ghci ":show paths"

-- | Perform a reload, list the messages that reload generated.
reload :: Ghci -> IO [Load]
reload ghci = parseLoad <$> exec ghci ":reload"

-- | Send @:quit@ and wait for the process to quit.
quit :: Ghci -> IO ()
quit ghci =  do
    interrupt ghci
    handle (\UnexpectedExit{} -> return ()) $ void $ exec ghci ":quit"
    -- Be aware that waitForProcess has a race condition, see https://github.com/haskell/process/issues/46.
    -- Therefore just ignore the exception anyway, its probably already terminated.
    ignored $ void $ waitForProcess $ process ghci


-- | Stop GHCi. Attempts to interrupt and execute @:quit:@, but if that doesn't complete
--   within 5 seconds it just terminates the process.
stopGhci :: Ghci -> IO ()
stopGhci ghci = do
    forkIO $ do
        -- if nicely doesn't work, kill ghci as the process level
        sleep 5
        terminateProcess $ process ghci
    quit ghci
