{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Control.Concurrent
import Control.Monad
import System.Directory
import Data.Time.Clock
import Data.List
import GHCi


foreign import stdcall unsafe "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO Int

foreign import stdcall unsafe "windows.h SetWindowPos"
    c_SetWindowPos :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int

c_HWND_TOPMOST = -1

main :: IO ()
main = do
    wnd <- c_GetConsoleWindow
    c_SetWindowPos wnd c_HWND_TOPMOST 0 0 0 0 3
    ghci <- ghci
    let fire msg warnings = do
            -- TODO: Persist file warnings that haven't changed by keeping (File,ModTime,Warnings)
            -- only show if the file is loaded
            start <- getCurrentTime
            load <- fmap parseLoad $ ghci msg
            modsActive <- fmap (map snd . parseShowModules) $ ghci ":show modules"
            modsLoad <- return $ nub $ map loadFile load
            load <- return $ load ++
                [w | w <- warnings, loadFile w `elem` modsActive, loadFile w `notElem` modsLoad]
            putStr $ unlines $ output $ filter isMessage load
            awaitFiles start $ nub $ modsLoad ++ modsActive
            fire ":reload" [m | m@Message{..} <- load, loadSeverity == Warning]
    fire "" []

output :: [Load] -> [String]
output [] = ["All good"]
output xs = map show xs


awaitFiles :: UTCTime -> [FilePath] -> IO ()
awaitFiles base files = do
    putStrLn $ "% WAITING: " ++ unwords files
    new <- mapM getModificationTime files
    when (all (< base) new) $ recheck new
    where
        recheck old = do
            sleep 0.1
            new <- mapM getModificationTime files
            when (old == new) $ recheck new


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000
