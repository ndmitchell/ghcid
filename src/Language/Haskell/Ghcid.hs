{-# LANGUAGE CPP #-}

-- | The entry point of the library
module Language.Haskell.Ghcid(
    Ghci, GhciError(..),
    Load(..), Severity(..),
    startGhci, stopGhci, interrupt,
    showModules, reload, exec, execTest
    ) where

import System.IO
import System.IO.Error
import System.Process
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Function
import Data.List
import Data.IORef
import Control.Applicative

import System.Console.CmdArgs.Verbosity
#if !defined(mingw32_HOST_OS)
import System.Posix.Signals
#endif

import Language.Haskell.Ghcid.Parser
import Language.Haskell.Ghcid.Types as T
import Language.Haskell.Ghcid.Util
import Prelude


-- | A GHCi session. Created with 'startGhci'.
data Ghci = Ghci (IO ())
                 (Bool -> String -> IO [String])


-- | Start GHCi, returning a function to perform further operation, as well as the result of the initial loading.
--   Pass True to write out messages produced while loading, useful if invoking something like "cabal repl"
--   which might compile dependent packages before really loading.
startGhci :: String -> Maybe FilePath -> Bool -> IO (Ghci, [Load])
startGhci cmd directory echo = do
    (Just inp, Just out, Just err, ph) <-
        createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, cwd=directory, create_group=True}

    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    lock <- newLock -- ensure only one person talks to ghci at a time
    let prefix = "#~GHCID-START~#"
    let finish = "#~GHCID-FINISH~#"
    hPutStrLn inp $ ":set prompt " ++ prefix
    hPutStrLn inp ":set -fno-break-on-exception -fno-break-on-error" -- see #43
    echo <- newIORef echo
    testRunning <- newIORef False

    -- consume from a handle, produce an MVar with either Just and a message, or Nothing (stream closed)
    let consume h name = do
            result <- newEmptyMVar -- the end result
            buffer <- newVar [] -- the things to go in result
            forkIO $ fix $ \rec -> do
                el <- tryBool isEOFError $ hGetLine h
                case el of
                    Left _ -> putMVar result Nothing
                    Right l -> do
                        whenLoud $ outStrLn $ "%" ++ name ++ ": " ++ l
                        whenM (readIORef echo) $
                            unless (any (`isInfixOf` l) [prefix, finish]) $ outStrLn l
                        if finish `isInfixOf` l
                          then do
                            buf <- modifyVar buffer $ \old -> return ([], reverse old)
                            putMVar result $ Just buf
                          else
                            modifyVar_ buffer $ return . (dropPrefixRepeatedly prefix l:)
                        rec
            return result

    outs <- consume out "GHCOUT"
    errs <- consume err "GHCERR"

    let f isTest s = do
            withLock lock $ do
                whenLoud $ outStrLn $ "%GHCINP: " ++ s
                when isTest $ writeIORef testRunning True
                hPutStrLn inp $ s ++ "\nPrelude.putStrLn " ++ show finish ++ "\nPrelude.error " ++ show finish
                outC <- takeMVar outs
                errC <- takeMVar errs
                when isTest $ writeIORef testRunning False
                case liftM2 (++) outC errC of
                    Nothing  -> do
                      if isTest
                        then throwIO $ UnexpectedExit cmd s
                        else return []
                    Just msg -> return msg

    let i = whenM (readIORef testRunning) $ do
                whenLoud $ outStrLn "%INTERRUPTED"
                ignore $ interruptProcessGroupOf ph
                writeIORef testRunning False

    let ghci = Ghci i f
#if !defined(mingw32_HOST_OS)
    tid <- myThreadId
    installHandler sigINT (Catch (i >> stopGhci ghci >> throwTo tid UserInterrupt)) Nothing
#endif
    r <- parseLoad <$> f False ""
    writeIORef echo False

    return (ghci, r)

-- | Show modules
showModules :: Ghci -> IO [(String,FilePath)]
showModules ghci = parseShowModules <$> exec ghci ":show modules"

-- | reload modules
reload :: Ghci -> IO [Load]
reload ghci = parseLoad <$> exec ghci ":reload"

-- | Stop GHCi
stopGhci :: Ghci -> IO ()
stopGhci ghci = handle (\UnexpectedExit{} -> return ()) $ void $ exec ghci ":quit"

-- | Send a command, get lines of result
exec :: Ghci -> String -> IO [String]
exec (Ghci _ f) = f False

execTest :: Ghci -> String -> IO [String]
execTest (Ghci _ f) = f True

-- | Interrupt Ghci
interrupt :: Ghci -> IO ()
interrupt (Ghci f _) = f
