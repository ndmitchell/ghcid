{-# LANGUAGE ScopedTypeVariables #-}
-- | The entry point of the library
module Language.Haskell.Ghcid
 ( T.Ghci
 , T.GhciError (..)
 , T.Severity (..)
 , T.Load (..)
 , startGhci
 , showModules
 , reload
 , exec
 , stopGhci
 )
where

import System.IO
import System.Process
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function
import Data.List

import System.Console.CmdArgs.Verbosity

import Language.Haskell.Ghcid.Parser
import Language.Haskell.Ghcid.Types as T
import Language.Haskell.Ghcid.Util

-- | Start GHCi, returning a function to perform further operation, as well as the result of the initial loading
startGhci :: String -> Maybe FilePath -> IO (Ghci, [Load])
startGhci cmd directory = do
    (Just inp, Just out, Just err, _) <-
        createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, cwd = directory}
    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    lock <- newMVar () -- ensure only one person talks to ghci at a time
    let prefix = "#~GHCID-START~#"
    let finish = "#~GHCID-FINISH~#"
    hPutStrLn inp $ ":set prompt " ++ prefix

    -- consume from a handle, produce an MVar with either Just and a message, or Nothing (stream closed)
    let consume h name = do
            result <- newEmptyMVar -- the end result
            buffer <- newMVar [] -- the things to go in result
            forkIO $ fix $ \rec -> do
                el <- try $ hGetLine h
                case el of
                    Left (_ :: SomeException) -> putMVar result Nothing
                    Right l -> do
                        whenLoud $ outStrLn $ "%" ++ name ++ ": " ++ l
                        if finish `isInfixOf` l
                          then do
                            buf <- modifyMVar buffer $ \old -> return ([], reverse old)
                            putMVar result $ Just buf
                          else
                            modifyMVar_ buffer $ return . (dropPrefixRepeatedly prefix l:)
                        rec
            return result

    outs <- consume out "GHCOUT"
    errs <- consume err "GHCERR"

    let f s = withMVar lock $ const $ do
                whenLoud $ outStrLn $ "%GHCINP: " ++ s
                hPutStrLn inp $ s ++ "\nputStrLn " ++ show finish ++ "\nerror " ++ show finish
                outC <- takeMVar outs
                errC <- takeMVar errs
                case liftM2 (++) outC errC of
                    Nothing  -> throwIO $ UnexpectedExit cmd s 
                    Just msg -> return  msg
    r <- fmap parseLoad $ f ""
    return (Ghci f,r)


-- | Show modules
showModules :: Ghci -> IO [(String,FilePath)]
showModules ghci = fmap parseShowModules $ exec ghci ":show modules"

-- | reload modules
reload :: Ghci -> IO [Load]
reload ghci = fmap parseLoad $ exec ghci ":reload"

-- | Stop GHCi
stopGhci :: Ghci -> IO ()
stopGhci ghci = handle (\UnexpectedExit {} -> return ()) $ void $ exec ghci ":quit"

-- | Send a command, get lines of result
exec :: Ghci -> String -> IO [String]
exec (Ghci x) = x
