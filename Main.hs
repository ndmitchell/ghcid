{-# LANGUAGE OverloadedStrings #-}
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
    let fire msg = do
            -- TODO: Persist file warnings that haven't changed by keeping (File,ModTime,Warnings)
            -- only show if the file is loaded
            start <- getCurrentTime
            load <- fmap parseLoad $ ghci msg
            mods <- fmap (map snd . parseShowModules) $ ghci ":show modules"
            putStr $ unlines $ reduceMessage load
            awaitFiles start $ nub $ mods -- ++ map loadFile result
            fire ":reload"
    fire ""


reduceMessage :: [String] -> [String]
reduceMessage = id


awaitFiles :: UTCTime -> [FilePath] -> IO ()
awaitFiles base files = do
    new <- mapM getModificationTime files
    when (all (< base) new) $ recheck new
    where
        recheck old = do
            sleep 0.1
            new <- mapM getModificationTime files
            when (old == new) $ recheck new


sleep :: Double -> IO ()
sleep x = threadDelay $ ceiling $ x * 1000000
