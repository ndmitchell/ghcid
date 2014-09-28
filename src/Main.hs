{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, ScopedTypeVariables #-}

module Main(main) where

import Control.Monad
import System.Directory
import Data.Time.Clock
import Data.List
import GHCi
import Util
import Test
import System.Console.CmdArgs
import Control.Exception


data Options = Options
    {command :: String
    ,height :: Int
    ,test :: Bool
    }
    deriving (Data,Typeable,Show)

options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,height = 8 &= help "Number of lines to show"
    ,test = False &= help "Run the test suite"
    } &= verbosity &=
    program "ghcid" &= summary "Auto :reload'ing GHCi daemon"

#if defined(mingw32_HOST_OS)
foreign import stdcall unsafe "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO Int

foreign import stdcall unsafe "windows.h SetWindowPos"
    c_SetWindowPos :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int

c_HWND_TOPMOST = -1
#endif

main :: IO ()
main = do
    Options{..} <- cmdArgsRun options
#if defined(mingw32_HOST_OS)
    when (not test) $ do
        wnd <- c_GetConsoleWindow
        c_SetWindowPos wnd c_HWND_TOPMOST 0 0 0 0 3
        return ()
#endif
    runTest test $ \output -> do
        dotGhci <- doesFileExist ".ghci"
        ghci <- ghci $ if command /= "" then command
                       else if dotGhci then "ghci" else "cabal repl"
        let fire msg warnings = do
                start <- getCurrentTime
                load <- fmap parseLoad $ ghci msg
                modsActive <- fmap (map snd . parseShowModules) $ ghci ":show modules"
                modsLoad <- return $ nub $ map loadFile load
                whenLoud $ do
                    outStrLn $ "%ACTIVE: " ++ show modsActive
                    outStrLn $ "%LOAD: " ++ show load
                warn <- return [w | w <- warnings, loadFile w `elem` modsActive, loadFile w `notElem` modsLoad]
                let outFill msg = output $ take height $ msg ++ replicate height ""
                outFill $ prettyOutput height $ filter isMessage load ++ warn
                reason <- awaitFiles start $ nub $ modsLoad ++ modsActive
                outFill $ "Reloading..." : map ("  " ++) reason
                fire ":reload" [m | m@Message{..} <- warn ++ load, loadSeverity == Warning]
        fire "" []


prettyOutput :: Int -> [Load] -> [String]
prettyOutput height [] = ["All good"]
prettyOutput height xs = take (height - (length msgs * 2)) msg1 ++ concatMap (take 2) msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map loadMessage err ++ map loadMessage warn


-- return a message about why you are continuing (usually a file name)
awaitFiles :: UTCTime -> [FilePath] -> IO [String]
awaitFiles base files = handle (\(e :: IOError) -> do sleep 0.1; return [show e]) $ do
    whenLoud $ outStrLn $ "% WAITING: " ++ unwords files
    new <- mapM getModificationTime files
    case [x | (x,t) <- zip files new, t > base] of
        [] -> recheck new
        xs -> return xs
    where
        recheck old = do
            sleep 0.1
            new <- mapM getModificationTime files
            case [x | (x,t1,t2) <- zip3 files old new, t1 /= t2] of
                [] -> recheck new
                xs -> return xs
