{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}

-- | The application entry point
module Ghcid(main, runGhcid) where

import Control.Applicative
import Control.Exception
import Control.Monad.Extra
import Control.Concurrent.Extra
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
    ,arguments :: [String]
    ,test :: Maybe String
    ,height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    ,notitle :: Bool
    ,restart :: [FilePath]
    ,directory :: FilePath
    ,outputfile :: [FilePath]
    }
    deriving (Data,Typeable,Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,arguments = [] &= args &= typ "MODULE"
    ,test = Nothing &= name "T" &= typ "EXPR" &= help "Command to run after successful loading"
    ,height = Nothing &= help "Number of lines to use (defaults to console height)"
    ,width = Nothing &= help "Number of columns to use (defaults to console width)"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    ,notitle = False &= help "Don't update the shell title/icon"
    ,restart = [] &= typFile &= help "Restart the command if any of these files change (defaults to .ghci or .cabal)"
    ,directory = "." &= typDir &= name "C" &= help "Set the current directory"
    ,outputfile = [] &= typFile &= name "o" &= help "File to write the full output to"
    } &= verbosity &=
    program "ghcid" &= summary ("Auto reloading GHCi daemon v" ++ showVersion version)


{-
What happens on various command lines:

Hlint with no .ghci file:
- cabal repl - prompt with Language.Haskell.HLint loaded
- cabal exec ghci Sample.hs - prompt with Sample.hs loaded
- ghci - prompt with nothing loaded
- ghci Sample.hs - prompt with Sample.hs loaded
- stack ghci - prompt with all libraries and Main loaded

Hlint with a .ghci file:
- cabal repl - loads everything twice, prompt with Language.Haskell.HLint loaded
- cabal exec ghci Sample.hs - loads everything first, then prompt with Sample.hs loaded
- ghci - prompt with everything
- ghci Sample.hs - loads everything first, then prompt with Sample.hs loaded
- stack ghci - loads everything first, then prompt with libraries and Main loaded

Warnings:
- cabal repl won't pull in any C files (e.g. hoogle)
- cabal exec ghci won't work with modules that import an autogen Paths module

As a result, we prefer to give users full control with a .ghci file, if available
-}
autoOptions :: Options -> IO Options
autoOptions o@Options{..}
    | command /= "" = return $ f [command] []
    | otherwise = do
        files <- getDirectoryContents "."
        let cabal = filter ((==) ".cabal" . takeExtension) files
        let opts = ["-fno-code" | isNothing test]
        return $ case () of
            _ | ".ghci" `elem` files -> f ("ghci":opts) [".ghci"]
              | "stack.yaml" `elem` files, False -> f ("stack ghci":map ("--ghci-options=" ++) opts) $ "stack.yaml":cabal -- see #130
              | cabal /= [] -> f (if arguments == [] then "cabal repl":map ("--ghc-options=" ++) opts else "cabal exec -- ghci":opts) cabal
              | otherwise -> f ("ghci":opts) []
    where
        f c r = o{command = unwords $ c ++ map escape arguments, arguments = [], restart = restart ++ r}

        -- in practice we're not expecting many arguments to have anything funky in them
        escape x | ' ' `elem` x = "\"" ++ x ++ "\""
                 | otherwise = x


-- ensure the action runs off the main thread
ctrlC :: IO () -> IO ()
ctrlC = join . onceFork


main :: IO ()
main = withWindowIcon $ ctrlC $ do
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
            handle (\(UnexpectedExit cmd _) -> putStrLn $ "Command \"" ++ cmd ++ "\" exited unexpectedly") $
                runGhcid waiter (nubOrd restart) command outputfile test height (not notitle) $ \xs -> do
                    outWith $ forM_ (groupOn fst xs) $ \x@((s,_):_) -> do
                        when (s == Bold) $ setSGR [SetConsoleIntensity BoldIntensity]
                        putStr $ concatMap ((:) '\n' . snd) x
                        when (s == Bold) $ setSGR []
                    hFlush stdout -- must flush, since we don't finish with a newline


data Style = Plain | Bold deriving Eq


runGhcid :: Waiter -> [FilePath] -> String -> [FilePath] -> Maybe String -> IO (Int,Int) -> Bool -> ([(Style,String)] -> IO ()) -> IO ()
runGhcid waiter restart command outputfiles test size titles output = do
    let outputFill :: Maybe (Int, [Load]) -> [String] -> IO ()
        outputFill load msg = do
            (width, height) <- size
            let n = height - length msg
            load <- return $ take (if isJust load then n else 0) $ prettyOutput (maybe 0 fst load)
                [ m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m}
                | m@Message{} <- maybe [] snd load]
            output $ load ++ map (Plain,) msg ++ replicate (height - (length load + length msg)) (Plain,"")

    restartTimes <- mapM getModTime restart
    outStrLn $ "Loading " ++ command ++ " ..."
    nextWait <- waitFiles waiter
    (ghci,messages) <- startGhci command Nothing True
    curdir <- getCurrentDirectory

    -- fire, given a waiter, the messages, and the warnings from last time
    let fire nextWait messages warnings = do
            let f m@Message{} = m{loadMessage = filter (not . ignoreMessageLine) $ loadMessage m}
                f x = x
            messages <- return $ map f $ filter (not . ignoreMessage) messages

            loaded <- map snd <$> showModules ghci
            let loadedCount = length loaded
            -- some may have reloaded, but caused an error, and thus not be in the loaded set
            let reloaded = nubOrd $ filter (/= "") $ map loadFile messages

            let wait = nubOrd $ loaded ++ reloaded
            whenLoud $ do
                outStrLn $ "%MESSAGES: " ++ show messages
                outStrLn $ "%LOADED: " ++ show loaded

            when (null wait && isNothing warnings) $ do
                putStrLn $ "\nNo files loaded, did not start GHCi properly.\nCommand: " ++ command
                exitFailure

            -- only keep old warnings from files that are still loaded, but did not reload
            let validWarn w = loadFile w `elem` loaded && loadFile w `notElem` reloaded
            -- newest warnings always go first, so the file you hit save on most recently has warnings first
            messages <- return $ messages ++ filter validWarn (fromMaybe [] warnings)
            let (countErrors, countWarnings) = both sum $ unzip
                    [if loadSeverity == Error then (1,0) else (0,1) | m@Message{..} <- messages, loadMessage /= []]
            test <- return $ if countErrors == 0 then test else Nothing

            when titles $ changeWindowIcon $
                if countErrors > 0 then IconError else if countWarnings > 0 then IconWarning else IconOK

            let updateTitle extra = when titles $ setTitle $
                    let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                    in (if countErrors == 0 && countWarnings == 0 then allGoodMessage else f countErrors "error" ++
                        (if countErrors > 0 && countWarnings > 0 then ", " else "") ++ f countWarnings "warning") ++
                       " " ++ extra ++ "- " ++ takeFileName curdir

            updateTitle $ if isJust test then "(running test) " else ""
            outputFill (Just (loadedCount, messages)) ["Running test..." | isJust test]
            forM_ outputfiles $ \file ->
                writeFile file $ unlines $ map snd $ prettyOutput loadedCount $ filter isMessage messages
            whenJust test $ \test -> do
                res <- exec ghci test
                outputFill (Just (loadedCount, messages)) $ fromMaybe res $ stripSuffix ["*** Exception: ExitSuccess"] res
                updateTitle ""

            when (null wait) $ do
                putStrLn $ "No files loaded, nothing to wait for. Fix the last error and restart."
                exitFailure
            reason <- nextWait $ restart ++ wait
            outputFill Nothing $ "Reloading..." : map ("  " ++) reason
            restartTimes2 <- mapM getModTime restart
            if restartTimes == restartTimes2 then do
                nextWait <- waitFiles waiter
                let warnings = [m | m@Message{..} <- messages, loadSeverity == Warning]
                messages <- reload ghci
                fire nextWait messages $ Just warnings
            else do
                stopGhci ghci
                runGhcid waiter restart command outputfiles test size titles output

    fire nextWait messages Nothing


-- | Ignore messages that GHC shouldn't really generate.
ignoreMessage :: Load -> Bool
ignoreMessage Message{loadSeverity=Warning, loadMessage=[_,x]}
    = x `elem` ["    -O conflicts with --interactive; -O ignored."]
ignoreMessage _ = False

-- | Ignore lines in messages that are pointless.
ignoreMessageLine :: String -> Bool
ignoreMessageLine x = any (`isPrefixOf` x) xs
    where
        xs = ["      except perhaps to import instances from"
             ,"    To import instances alone, use: import "]


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: Int -> [Load] -> [(Style,String)]
prettyOutput loaded [] = [(Plain,allGoodMessage ++ " (" ++ show loaded ++ " module" ++ ['s' | loaded /= 1] ++ ")")]
prettyOutput loaded xs = concat $ msg1:msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map (map (Bold,) . loadMessage) err ++ map (map (Plain,) . loadMessage) warn
