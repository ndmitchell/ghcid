{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP #-}

module Main(main) where

import Control.Concurrent
import System.Directory
import Data.Time.Clock
import Data.List
import GHCi
import Util
import System.Console.CmdArgs

data Options = Options
    {command :: String
    ,height :: Int
    }
    deriving (Data,Typeable,Show)

options = cmdArgsMode $ Options
    {command = "ghci" &= help "Command to run (defaults to ghci)"
    ,height = 8 &= help "Number of lines to show"
    } &= verbosity

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
    wnd <- c_GetConsoleWindow
    c_SetWindowPos wnd c_HWND_TOPMOST 0 0 0 0 3
#endif
    ghci <- ghci command
    let fire msg warnings = do
            start <- getCurrentTime
            load <- fmap parseLoad $ ghci msg
            modsActive <- fmap (map snd . parseShowModules) $ ghci ":show modules"
            modsLoad <- return $ nub $ map loadFile load
            whenLoud $ do
                outStrLn $ "%ACTIVE: " ++ show modsActive
                outStrLn $ "%LOAD: " ++ show load
            warn <- return [w | w <- warnings, loadFile w `elem` modsActive, loadFile w `notElem` modsLoad]
            let msg = prettyOutput height $ filter isMessage load ++ warn
            outStr $ unlines $ take height $ msg ++ replicate height "" 
            reason <- awaitFiles start $ nub $ modsLoad ++ modsActive
            outStr $ unlines $ "Reloading..." : ("  " ++ reason) : replicate (height - 2) ""
            fire ":reload" [m | m@Message{..} <- warn ++ load, loadSeverity == Warning]
    fire "" []


prettyOutput :: Int -> [Load] -> [String]
prettyOutput height [] = ["All good"]
prettyOutput height xs = take (height - (length msgs * 2)) msg1 ++ concatMap (take 2) msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map loadMessage err ++ map loadMessage warn


-- return a message about why you are continuing (usually a file name)
awaitFiles :: UTCTime -> [FilePath] -> IO String
awaitFiles base files = do
    whenLoud $ outStrLn $ "% WAITING: " ++ unwords files
    new <- mapM getModificationTime files
    case [x | (x,t) <- zip files new, t > base] of
        x:_ -> return x
        [] -> recheck new
    where
        recheck old = do
            sleep 0.1
            new <- mapM getModificationTime files
            case [x | (x,t1,t2) <- zip3 files old new, t1 /= t2] of
                x:_ -> return x
                [] -> recheck new


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000
