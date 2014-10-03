{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, ScopedTypeVariables #-}
-- | This is the meat of the executable. It's not in Main so it can be called by the test suite.
module Language.Haskell.GhcidProgram where

import Data.List
import Data.Time.Clock

import System.Directory
import System.Console.CmdArgs
import System.IO.Error
import Control.Exception

import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Types
import Language.Haskell.Ghcid.Util

-- | Command line options
data Options = Options
    {command :: String
    ,height :: Int
    ,topmost :: Bool
    }
    deriving (Data,Typeable,Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,height = 8 &= help "Number of lines to show"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    } &= verbosity &=
    program "ghcid" &= summary "Auto :reload'ing GHCi daemon"
    

runGhcid :: forall b a. String -> Int -> ([String] -> IO a) -> IO b
runGhcid command height output = do
      dotGhci <- doesFileExist ".ghci"
      (ghci,initLoad) <- startGhci (if command /= "" then command
                     else if dotGhci then "ghci" else "cabal repl") Nothing
      let fire load warnings = do
              start <- getCurrentTime
              modsActive <- fmap (map snd) $ showModules ghci
              let modsLoad = nub $ map loadFile load
              whenLoud $ do
                  outStrLn $ "%ACTIVE: " ++ show modsActive
                  outStrLn $ "%LOAD: " ++ show load
              let warn = [w | w <- warnings, loadFile w `elem` modsActive, loadFile w `notElem` modsLoad]
              let outFill msg = output $ take height $ msg ++ replicate height ""
              outFill $ prettyOutput height $ filter isMessage load ++ warn
              reason <- awaitFiles start $ nub $ modsLoad ++ modsActive
              outFill $ "Reloading..." : map ("  " ++) reason
              load2 <- reload ghci
              fire load2 [m | m@Message{..} <- warn ++ load, loadSeverity == Warning]
      fire initLoad []
      
prettyOutput :: Int -> [Load] -> [String]
prettyOutput _ [] = ["All good"]
prettyOutput height xs = take (height - (length msgs * 2)) msg1 ++ concatMap (take 2) msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map loadMessage err ++ map loadMessage warn


-- return a message about why you are continuing (usually a file name)
awaitFiles :: UTCTime -> [FilePath] -> IO [String]
awaitFiles base files = handle (\(e :: IOError) -> do sleep 0.1; return [show e]) $ do
    whenLoud $ outStrLn $ "%WAITING: " ++ unwords files
    new <- mapM mtime files
    case [x | (x,Just t) <- zip files new, t > base] of
        [] -> recheck files new
        xs -> return xs
    where
        recheck files' old = do
            sleep 0.1
            new <- mapM mtime files'
            case [x | (x,t1,t2) <- zip3 files' old new, t1 /= t2] of
                [] -> recheck files' new
                xs -> return xs

mtime :: FilePath -> IO (Maybe UTCTime)
mtime file = handleJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (\_ -> return Nothing)
    (fmap Just $ getModificationTime file)
      