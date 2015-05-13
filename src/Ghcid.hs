{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, TupleSections #-}
{-# OPTIONS_GHC -O2 -fno-cse #-} -- only here to test ticket #11

-- | The application entry point
module Ghcid(main, runGhcid) where

import Control.Applicative
import Control.Monad.Extra
import Control.Concurrent.Extra
import Control.Exception
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
import Language.Haskell.Ghcid.Types
import Wait
import Prelude


-- | Command line options
data Options = Options
    {command :: String
    ,test :: Maybe String
    ,height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    ,restart :: [FilePath]
    ,directory :: FilePath
    ,outputfile :: [FilePath]
    }
    deriving (Data,Typeable,Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,test = Nothing &= name "T" &= typ "EXPR" &= help "Command to run after successful loading"
    ,height = Nothing &= help "Number of lines to show (defaults to console height)"
    ,width = Nothing &= help "Number of columns to show (defaults to console width)"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    ,restart = [] &= typFile &= help "Restart the command if any of these files change (defaults to .ghci or .cabal)"
    ,directory = "." &= typDir &= name "C" &= help "Set the current directory"
    ,outputfile = [] &= typFile &= name "o" &= help "File to write the full output to"
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


ctrlC :: IO () -> IO ()
ctrlC act = do
    bar <- newBarrier
    forkFinally act $ signalBarrier bar
    either throwIO return =<< waitBarrier bar


main :: IO ()
main = ctrlC $ do
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
            runGhcid waiter restart command outputfile test height $ \xs -> do
                outWith $ forM_ (groupOn fst xs) $ \x@((s,_):_) -> do
                    when (s == Bold) $ setSGR [SetConsoleIntensity BoldIntensity]
                    putStr $ concatMap ((:) '\n' . snd) x
                    when (s == Bold) $ setSGR []
                hFlush stdout -- must flush, since we don't finish with a newline


data Style = Plain | Bold deriving Eq


runGhcid :: Waiter -> [FilePath] -> String -> [FilePath] -> Maybe String -> IO (Int,Int) -> ([(Style,String)] -> IO ()) -> IO ()
runGhcid waiter restart command outputfiles test size output = do
    let outputFill :: Maybe [Load] -> [String] -> IO ()
        outputFill load msg = do
            (width, height) <- size
            let n = height - length msg
            load <- return $ take (if isJust load then n else 0) $ prettyOutput n
                [ m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m}
                | m@Message{} <- fromMaybe [] load]
            output $ load ++ map (Plain,) msg ++ replicate (height - (length load + length msg)) (Plain,"")

    restartTimes <- mapM getModTime restart
    outputFill Nothing ["Loading..."]
    nextWait <- waitFiles waiter
    (ghci,messages) <- startGhci command Nothing
    curdir <- getCurrentDirectory

    -- fire, given a waiter, the messages, and the warnings from last time
    let fire nextWait messages warnings = do
            messages <- return $ filter (not . whitelist) messages

            loaded <- map snd <$> showModules ghci
            let reloaded = nubOrd $ map loadFile messages
            -- some may have reloaded, but caused an error, and thus not be in the loaded set
            whenLoud $ do
                outStrLn $ "%MESSAGES: " ++ show messages
                outStrLn $ "%LOADED: " ++ show loaded

            -- only keep old warnings from files that are still loaded, but did not reload
            let validWarn w = loadFile w `elem` loaded && loadFile w `notElem` reloaded
            -- newest warnings always go first, so the file you hit save on most recently has warnings first
            messages <- return $ messages ++ filter validWarn warnings
            let (countErrors, countWarnings) = both sum $ unzip [if loadSeverity m == Error then (1,0) else (0,1) | m@Message{} <- messages]
            test <- return $ if countErrors == 0 then test else Nothing

            let updateTitle extra = setTitle $
                    let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                    in (if countErrors == 0 && countWarnings == 0 then allGoodMessage else f countErrors "error" ++
                        (if countErrors > 0 && countWarnings > 0 then ", " else "") ++ f countWarnings "warning") ++
                       " " ++ extra ++ "- " ++ takeFileName curdir

            updateTitle $ if isJust test then "(running test) " else ""
            outputFill (Just messages) ["Running test..." | isJust test]
            forM_ outputfiles $ \file ->
                writeFile file $ unlines $ map snd $ prettyOutput 1000000 $ filter isMessage messages
            whenJust test $ \test -> do
                res <- exec ghci test
                outputFill (Just messages) $ fromMaybe res $ stripSuffix ["*** Exception: ExitSuccess"] res
                updateTitle ""

            let wait = nubOrd $ loaded ++ reloaded
            when (null wait) $ do
                putStrLn $ "No files loaded, probably did not start GHCi.\nCommand: " ++ command
                exitFailure
            reason <- nextWait $ restart ++ wait
            outputFill Nothing $ "Reloading..." : map ("  " ++) reason
            restartTimes2 <- mapM getModTime restart
            if restartTimes == restartTimes2 then do
                nextWait <- waitFiles waiter
                let warnings = [m | m@Message{..} <- messages, loadSeverity == Warning]
                messages <- reload ghci
                fire nextWait messages warnings
            else do
                stopGhci ghci
                runGhcid waiter restart command outputfiles test size output

    fire nextWait messages []


-- | Ignore messages that GHC shouldn't really generate.
whitelist :: Load -> Bool
whitelist Message{loadSeverity=Warning, loadMessage=[_,x]}
    = x `elem` ["    -O conflicts with --interactive; -O ignored."]
whitelist _ = False


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: Int -> [Load] -> [(Style,String)]
prettyOutput height [] = [(Plain,allGoodMessage)]
prettyOutput height xs = take (max 3 $ height - (length msgs * 2)) msg1 ++ concatMap (take 2) msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map (map (Bold,) . loadMessage) err ++ map (map (Plain,) . loadMessage) warn
