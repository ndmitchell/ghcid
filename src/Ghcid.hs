{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -O2 #-} -- only here to test ticket #11

-- | The application entry point
module Ghcid(main, runGhcid) where

import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Data.Version
import qualified System.Console.Terminal.Size as Term
import System.Console.CmdArgs
import System.Console.ANSI
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO

import Paths_ghcid
import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Terminal
import Language.Haskell.Ghcid.Util
import Wait


-- | Command line options
data Options = Options
    {command :: String
    ,height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    ,restart :: [FilePath]
    ,directory :: FilePath
    }
    deriving (Data,Typeable,Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,height = Nothing &= help "Number of lines to show (defaults to console height)"
    ,width = Nothing &= help "Number of columns to show (defaults to console width)"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    ,restart = [] &= typFile &= help "Restart the command if any of these files change (defaults to .ghci or .cabal)"
    ,directory = "." &= typDir &= name "C" &= help "Set the current directory"
    } &= verbosity &=
    program "ghcid" &= summary ("Auto reloading GHCi daemon v" ++ showVersion version)


autoOptions :: Options -> IO Options
autoOptions o
    | command o /= "" = return o
    | otherwise = do
        files <- getDirectoryContents "."
        let cabal = filter ((==) ".cabal" . takeExtension) files
        if null cabal || ".ghci" `elem` files
            then return o{command="ghci", restart=[".ghci"]}
            else return o{command="cabal repl", restart=cabal}


main :: IO ()
main = do
    opts <- cmdArgsRun options
    withCurrentDirectory (directory opts) $ do
        opts@Options{..} <- autoOptions opts
        when topmost terminalTopmost
        height <- return $ case (width, height) of
            (Just w, Just h) -> return (w,h)
            _ -> do
                term <- fmap (fmap $ Term.width &&& Term.height) Term.size
                whenLoud $ do
                    outStrLn $ "%CONSOLE: width = " ++ maybe "?" (show . fst) term ++ ", height = " ++ maybe "?" (show . snd) term
                let f user def sel = fromMaybe (maybe def sel term) user
                -- if we write to the end of the window then it wraps automatically
                -- so putStrLn width 'x' uses up two lines
                return (f width 80 (pred . fst), f height 8 snd)
        withWaiterNotify $ \waiter ->
            runGhcid waiter restart command height $ \xs -> do
                outWith $ forM_ (groupOn fst xs) $ \x@((b,_):_) -> do
                    when b $ setSGR [SetConsoleIntensity BoldIntensity]
                    putStr $ concatMap ((:) '\n' . snd) x
                    when b $ setSGR []
                hFlush stdout -- must flush, since we don't finish with a newline


runGhcid :: Waiter -> [FilePath] -> String -> IO (Int,Int) -> ([(Bool,String)] -> IO ()) -> IO ()
runGhcid waiter restart command size output = do
    let outputFill :: Maybe [Load] -> [String] -> IO ()
        outputFill load msg = do
            (width, height) <- size
            let n = height - length msg
            load <- return $ take (if isJust load then n else 0) $ prettyOutput n
                [ m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m}
                | m@Message{} <- fromMaybe [] load]
            output $ load ++ map (False,) msg ++ replicate (height - (length load + length msg)) (False,"")

    restartTimes <- mapM getModTime restart
    outputFill Nothing ["Loading..."]
    nextWait <- waitFiles waiter
    (ghci,messages) <- startGhci command Nothing
    curdir <- getCurrentDirectory

    -- fire, given a waiter, the messages, and the warnings from last time
    let fire nextWait messages warnings = do
            messages <- return $ filter (not . whitelist) messages
            modsActive <- fmap (map snd) $ showModules ghci
            let modsLoad = nubOrd $ map loadFile messages
            whenLoud $ do
                outStrLn $ "%ACTIVE: " ++ show modsActive
                outStrLn $ "%LOAD: " ++ show messages
            let warn = [w | w <- warnings, loadFile w `elem` modsActive, loadFile w `notElem` modsLoad]
            (width, height) <- size
            outputFill (Just $ warn ++ messages) []
            setTitle $
                let (errs, warns) = both sum $ unzip [if loadSeverity m == Error then (1,0) else (0,1) | m@Message{} <- messages ++ warn]
                    f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                in (if errs == 0 && warns == 0 then "All good" else f errs "error" ++
                    (if errs > 0 && warns > 0 then ", " else "") ++ f warns "warning") ++
                   " - " ++ takeFileName curdir
            let wait = nubOrd $ modsLoad ++ modsActive
            when (null wait) $ do
                putStrLn $ "No files loaded, probably did not start GHCi.\nCommand: " ++ command
                exitFailure
            reason <- nextWait $ restart ++ wait
            outputFill Nothing $ "Reloading..." : map ("  " ++) reason
            restartTimes2 <- mapM getModTime restart
            if restartTimes == restartTimes2 then do
                nextWait <- waitFiles waiter
                load2 <- reload ghci
                fire nextWait load2 [m | m@Message{..} <- warn ++ messages, loadSeverity == Warning]
            else do
                stopGhci ghci
                runGhcid waiter restart command size output
    fire nextWait messages []


-- | Ignore messages that GHC shouldn't really generate.
whitelist :: Load -> Bool
whitelist Message{loadSeverity=Warning, loadMessage=[_,x]}
    = x `elem` ["    -O conflicts with --interactive; -O ignored."]
whitelist _ = False


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: Int -> [Load] -> [(Bool,String)]
prettyOutput height [] = [(False,allGoodMessage)]
prettyOutput height xs = take (max 3 $ height - (length msgs * 2)) msg1 ++ concatMap (take 2) msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map (map (True,) . loadMessage) err ++ map (map (False,) . loadMessage) warn
