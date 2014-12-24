{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O2 #-} -- only here to test ticket #11

-- | The application entry point
module Ghcid(main, runGhcid) where

import Control.Exception
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Data.Time.Clock
import Data.Tuple.Extra
import Data.Version
import qualified System.Console.Terminal.Size as Term
import System.Console.CmdArgs
import System.Directory
import System.Exit
import System.IO
import System.IO.Error
import System.Time.Extra

import Paths_ghcid
import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Terminal
import Language.Haskell.Ghcid.Util


-- | Command line options
data Options = Options
    {command :: String
    ,height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    }
    deriving (Data,Typeable,Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,height = Nothing &= help "Number of lines to show (defaults to console height)"
    ,width = Nothing &= help "Number of columns to show (defaults to console width)"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    } &= verbosity &=
    program "ghcid" &= summary ("Auto reloading GHCi daemon v" ++ showVersion version)


main :: IO ()
main = do
    opts@Options{..} <- cmdArgsRun options
    when topmost terminalTopmost
    height <- return $ case (width, height) of
        (Just w, Just h) -> return (w,h)
        _ -> do
            term <- fmap (fmap $ Term.width &&& Term.height) Term.size
            let f user def sel = fromMaybe (maybe def sel term) user
            -- if we write to the end of the window then it wraps automatically
            -- so putStrLn width 'x' uses up two lines
            return (f width 80 (pred . fst), f height 8 snd)
    command <- if command /= "" then return command else
               ifM (doesFileExist ".ghci") (return "ghci") (return "cabal repl")
    runGhcid command height $ \xs -> do
        outStr $ concatMap ('\n':) xs
        hFlush stdout -- must flush, since we don't finish with a newline


runGhcid :: String -> IO (Int,Int) -> ([String] -> IO ()) -> IO ()
runGhcid command size output = do
    do (_,height) <- size; output $ "Loading..." : replicate (height - 1) ""
    (ghci,initLoad) <- startGhci command Nothing
    let fire load warnings = do
            load <- return $ filter (not . whitelist) load
            (width, height) <- size
            start <- getCurrentTime
            modsActive <- fmap (map snd) $ showModules ghci
            let modsLoad = nub $ map loadFile load
            whenLoud $ do
                outStrLn $ "%ACTIVE: " ++ show modsActive
                outStrLn $ "%LOAD: " ++ show load
            let warn = [w | w <- warnings, loadFile w `elem` modsActive, loadFile w `notElem` modsLoad]
            let outFill msg = output $ take height $ msg ++ replicate height ""
            outFill $ prettyOutput height
                [m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m} | m@Message{} <- load ++ warn]
            let wait = nub $ modsLoad ++ modsActive
            when (null wait) $ do
                putStrLn $ "No files loaded, probably did not start GHCi.\nCommand: " ++ command
                exitFailure
            reason <- awaitFiles start wait
            outFill $ "Reloading..." : map ("  " ++) reason
            load2 <- reload ghci
            fire load2 [m | m@Message{..} <- warn ++ load, loadSeverity == Warning]
    fire initLoad []


whitelist :: Load -> Bool
whitelist Message{loadSeverity=Warning, loadMessage=[_,x]}
    = x `elem` ["    -O conflicts with --interactive; -O ignored."]
whitelist _ = False


prettyOutput :: Int -> [Load] -> [String]
prettyOutput _ [] = [allGoodMessage]
prettyOutput height xs = take (max 3 $ height - (length msgs * 2)) msg1 ++ concatMap (take 2) msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map loadMessage err ++ map loadMessage warn


-- | return a message about why you are continuing (usually a file name)
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
