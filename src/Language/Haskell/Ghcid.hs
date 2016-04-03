{-# LANGUAGE RecordWildCards #-}

-- | Library for spawning and working with Ghci sessions.
module Language.Haskell.Ghcid(
    Ghci, GhciError(..), Stream(..),
    Load(..), Severity(..),
    startGhci, stopGhci, interrupt, process, execStream,
    showModules, reload, exec, quit
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
    ,ghciInterupt :: IO ()
    ,ghciExec :: String -> (Stream -> String -> IO ()) -> IO ()}


-- | Start GHCi, returning a function to perform further operation, as well as the result of the initial loading.
--   If you do not call 'stopGhci' then the underlying process may be leaked.
--   The callback will be given the messages produced while loading, useful if invoking something like "cabal repl"
--   which might compile dependent packages before really loading.
startGhci :: String -> Maybe FilePath -> (Stream -> String -> IO ()) -> IO (Ghci, [Load])
startGhci cmd directory echo0 = do
    (Just inp, Just out, Just err, ghciProcess) <-
        createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, cwd=directory, create_group=True}

    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    -- I'd like the GHCi prompt to go away, but that's not possible, so I set it to a special
    -- string and filter that out.
    let ghcid_prefix = "#~GHCID-START~#"
    let removePrefix = dropPrefixRepeatedly ghcid_prefix
    hPutStrLn inp $ ":set prompt " ++ ghcid_prefix
    hPutStrLn inp ":set -fno-break-on-exception -fno-break-on-error" -- see #43

    -- At various points I need to ensure everything the user is waiting for has completed
    -- So I send messages on stdout/stderr and wait for them to arrive
    syncCount <- newVar 0
    let syncReplay = do
            i <- readVar syncCount
            let msg = "#~GHCID-FINISH-" ++ show i ++ "~#"
            hPutStrLn inp $ "Prelude.putStrLn " ++ show msg ++ "\nPrelude.error " ++ show msg
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

    let consume2 :: (Stream -> String -> IO (Maybe a)) -> IO (Maybe (a,a))
        consume2 finish = do
            res1 <- onceFork $ consume Stdout (finish Stdout)
            res2 <- consume Stderr (finish Stderr)
            res1 <- res1
            return $ liftM2 (,) res1 res2


    -- held while interrupting, and briefly held when starting an exec
    -- ensures exec values queue up behind an ongoing interrupt and no two interrupts run at once
    isInterrupting <- newLock

    -- is anyone running running an exec statement, ensure only one person talks to ghci at a time
    isRunning <- newLock

    let ghciExec command echo = do
            withLock isInterrupting $ return ()
            res <- withLockTry isRunning $ do
                whenLoud $ outStrLn $ "%GHCINP: " ++ command
                hPutStrLn inp command
                stop <- syncFresh
                res <- consume2 $ \strm s ->
                    if stop s then return $ Just () else do echo strm s; return Nothing
                when (res == Nothing) $
                    throwIO $ UnexpectedExit cmd command
            when (isNothing res) $
                fail "Ghcid.exec, computation is already running, must be used single-threaded"

    let ghciInterupt = withLock isInterrupting $ do
            whenM (fmap isNothing $ withLockTry isRunning $ return ()) $ do
                whenLoud $ outStrLn "%INTERRUPT"
                interruptProcessGroupOf ghciProcess
                syncReplay -- let the running person finish
                stop <- syncFresh -- now sync on a fresh message (in case they finished before)
                res <- consume2 $ \_ s -> return $ if stop s then Just () else Nothing
                when (res == Nothing) $
                    throwIO $ UnexpectedExit cmd "Interrupt"
                withLock isRunning $ return ()

    let ghci = Ghci{..}
    r <- parseLoad <$> execBuffer ghci "" echo0
    return (ghci, r)


-- | Execute a command, calling a callback on each response.
--   The callback will be called single threaded.
execStream :: Ghci -> String -> (Stream -> String -> IO ()) -> IO ()
execStream = ghciExec

-- | Interrupt Ghci, stopping the current computation (if any),
--   but leaving the process open to new input.
interrupt :: Ghci -> IO ()
interrupt = ghciInterupt

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
    execStream ghci cmd $ \i s -> do
        modifyIORef (if i == Stdout then stdout else stderr) (s:)
        echo i s
    reverse <$> ((++) <$> readIORef stderr <*> readIORef stdout)

-- | Send a command, get lines of result. Must be called single-threaded.
exec :: Ghci -> String -> IO [String]
exec ghci cmd = execBuffer ghci cmd $ \_ _ -> return ()

-- | List the modules currently loaded, with module name and source file.
showModules :: Ghci -> IO [(String,FilePath)]
showModules ghci = parseShowModules <$> exec ghci ":show modules"

-- | Perform a reload, list the messages that reload generated.
reload :: Ghci -> IO [Load]
reload ghci = parseLoad <$> exec ghci ":reload"

-- | Send @:quit@ and wait for the process to quit.
quit :: Ghci -> IO ()
quit ghci =  do
    interrupt ghci
    handle (\UnexpectedExit{} -> return ()) $ void $ exec ghci ":quit"
    void $ waitForProcess $ process ghci


-- | Stop GHCi. Attempts to interrupt and execute @:quit:@, but if that doesn't complete
--   within 5 seconds it just terminates the process.
stopGhci :: Ghci -> IO ()
stopGhci ghci = do
    forkIO $ quit ghci
    forkIO $ do
        -- if nicely doesn't work, kill ghci as the process level
        sleep 5
        terminateProcess $ process ghci
    void $ waitForProcess $ process ghci
