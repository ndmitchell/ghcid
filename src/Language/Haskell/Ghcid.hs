{-# LANGUAGE RecordWildCards #-}

-- | The entry point of the library
module Language.Haskell.Ghcid(
    Ghci, GhciError(..), Stream(..),
    Load(..), Severity(..),
    startGhci, stopGhci, interrupt, execStream,
    showModules, reload, exec
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
startGhci cmd directory echoer = do
    (Just inp, Just out, Just err, ghciProcess) <-
        createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, cwd=directory, create_group=True}

    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    let prefix = "#~GHCID-START~#"
    let finish = "#~GHCID-FINISH~#"
    hPutStrLn inp $ ":set prompt " ++ prefix
    hPutStrLn inp ":set -fno-break-on-exception -fno-break-on-error" -- see #43

    lock <- newLock -- ensure only one person talks to ghci at a time
    echo <- newVar echoer -- where to write the output
    isRunning <- newIORef False

    -- consume from a handle
    -- produce an MVar with either False (computation finished), or True (stream closed)
    -- send all data collected to echo
    let consume h name = do
            result <- newEmptyMVar -- the end result
            forkIO $ fix $ \rec -> do
                el <- tryBool isEOFError $ hGetLine h
                case el of
                    Left _ -> putMVar result True
                    Right l -> do
                        whenLoud $ outStrLn $ "%" ++ upper (show name) ++ ": " ++ l
                        if finish `isInfixOf` l then
                            putMVar result False
                         else do
                            withVar echo $ \echo -> echo name $ dropPrefixRepeatedly prefix l
                        rec
            return result

    outs <- consume out Stdout
    errs <- consume err Stderr

    let ghciExec s echoer = do
            withLock lock $ do
                modifyVar_ echo $ \old -> return echoer
                whenLoud $ outStrLn $ "%GHCINP: " ++ s
                writeIORef isRunning True
                hPutStrLn inp $ s ++ "\nPrelude.putStrLn " ++ show finish ++ "\nPrelude.error " ++ show finish
                outC <- takeMVar outs
                errC <- takeMVar errs
                writeIORef isRunning False
                when (outC || errC) $
                    throwIO $ UnexpectedExit cmd s

    let ghciInterupt = whenM (readIORef isRunning) $ do
                whenLoud $ outStrLn "%INTERRUPTED"
                ignore $ interruptProcessGroupOf ghciProcess
                writeIORef isRunning False

    let ghci = Ghci{..}
    r <- parseLoad <$> exec ghci ""
    return (ghci, r)

-- | Stop GHCi
stopGhci :: Ghci -> IO ()
stopGhci ghci = do
    handle (\UnexpectedExit{} -> return ()) $ void $ exec ghci ":quit"
    void $ forkIO $ ignore $ do
        sleep 5 -- try and give ghci a chance to go quietly
        terminateProcess $ ghciProcess ghci
    void $ waitForProcess $ ghciProcess ghci

-- | Execute a command, calling a callback on each response.
--   The callback will be called single threaded.
execStream :: Ghci -> String -> (Stream -> String -> IO ()) -> IO ()
execStream = ghciExec

-- | Interrupt Ghci, stopping the current task, but leaving the process open to new input.
interrupt :: Ghci -> IO ()
interrupt = ghciInterupt


---------------------------------------------------------------------
-- SUGAR HELPERS

-- | Send a command, get lines of result
exec :: Ghci -> String -> IO [String]
exec ghci cmd = do
    stdout <- newIORef []
    stderr <- newIORef []
    execStream ghci cmd $ \i s -> modifyIORef (if i == Stdout then stdout else stderr) (s:)
    reverse <$> ((++) <$> readIORef stderr <*> readIORef stdout)

-- | Show modules
showModules :: Ghci -> IO [(String,FilePath)]
showModules ghci = parseShowModules <$> exec ghci ":show modules"

-- | reload modules
reload :: Ghci -> IO [Load]
reload ghci = parseLoad <$> exec ghci ":reload"
