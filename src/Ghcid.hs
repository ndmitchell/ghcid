{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}

-- | The application entry point
module Ghcid(main, runGhcid) where

import Control.Exception
import System.IO.Error
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Data.Version
import Session
import qualified System.Console.Terminal.Size as Term
import System.Console.CmdArgs
import System.Console.ANSI
import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe

import Paths_ghcid
import Language.Haskell.Ghcid
import Language.Haskell.Ghcid.Terminal
import Language.Haskell.Ghcid.Util
import Language.Haskell.Ghcid.Types
import Wait


-- | Command line options
data Options = Options
    {command :: String
    ,arguments :: [String]
    ,test :: Maybe String
    ,height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    ,notitle :: Bool
    ,reload :: [FilePath]
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
    ,restart = [] &= typ "PATH" &= help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file)"
    ,reload = [] &= typ "PATH" &= help "Reload when the given file or directory contents change (defaults to none)"
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

        -- use unsafePerformIO to get nicer pattern matching for logic (read-only operations)
        let isStack dir = unsafePerformIO $ flip catchIOError (const $ return False) $
                doesFileExist (dir </> "stack.yaml") &&^ doesDirectoryExist (dir </> ".stack-work")

        let cabal = filter ((==) ".cabal" . takeExtension) files
        let opts = ["-fno-code" | isNothing test]
        return $ case () of
            _ | cabal /= [], isStack "." || isStack ".." -> -- stack file might be parent, see #62
                let flags = if null arguments then
                                "stack ghci --test" :
                                ["--no-load" | ".ghci" `elem` files] ++
                                map ("--ghci-options=" ++) opts
                            else
                                "stack exec --test --" : opts
                in f flags $ "stack.yaml":cabal
              | ".ghci" `elem` files -> f ("ghci":opts) [".ghci"]
              | cabal /= [] -> f (if arguments == [] then "cabal repl":map ("--ghc-options=" ++) opts else "cabal exec -- ghci":opts) cabal
              | otherwise -> f ("ghci":opts) []
    where
        f c r = o{command = unwords $ c ++ map escape arguments, arguments = [], restart = restart ++ r}

        -- in practice we're not expecting many arguments to have anything funky in them
        escape x | ' ' `elem` x = "\"" ++ x ++ "\""
                 | otherwise = x


main :: IO ()
main = withWindowIcon $ withSession $ \session -> do
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
                runGhcid session waiter (nubOrd restart) (nubOrd reload) command outputfile test height (not notitle) $ \xs -> do
                    outWith $ forM_ (groupOn fst xs) $ \x@((s,_):_) -> do
                        when (s == Bold) $ setSGR [SetConsoleIntensity BoldIntensity]
                        putStr $ concatMap ((:) '\n' . snd) x
                        when (s == Bold) $ setSGR []
                    hFlush stdout -- must flush, since we don't finish with a newline


data Style = Plain | Bold deriving Eq


runGhcid :: Session -> Waiter -> [FilePath] -> [FilePath] -> String -> [FilePath] -> Maybe String -> IO (Int,Int) -> Bool -> ([(Style,String)] -> IO ()) -> IO ()
runGhcid session waiter restart reload command outputfiles test size titles output = do
    let outputFill :: Maybe (Int, [Load]) -> [String] -> IO ()
        outputFill load msg = do
            (width, height) <- size
            let n = height - length msg
            load <- return $ take (if isJust load then n else 0) $ prettyOutput (maybe 0 fst load)
                [ m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m}
                | m@Message{} <- maybe [] snd load]
            output $ load ++ map (Plain,) msg ++ replicate (height - (length load + length msg)) (Plain,"")

    restartTimes <- mapM getModTime restart
    curdir <- getCurrentDirectory

    -- fire, given a waiter, the messages/loaded
    let fire nextWait (messages, loaded) = do
            let loadedCount = length loaded
            whenLoud $ do
                outStrLn $ "%MESSAGES: " ++ show messages
                outStrLn $ "%LOADED: " ++ show loaded

            let (countErrors, countWarnings) = both sum $ unzip
                    [if loadSeverity == Error then (1,0) else (0,1) | m@Message{..} <- messages, loadMessage /= []]
            test <- return $ if countErrors == 0 && countWarnings == 0 then test else Nothing

            when titles $ setWindowIcon $
                if countErrors > 0 then IconError else if countWarnings > 0 then IconWarning else IconOK

            let updateTitle extra = when titles $ setTitle $
                    let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                    in (if countErrors == 0 && countWarnings == 0 then allGoodMessage else f countErrors "error" ++
                        (if countErrors > 0 && countWarnings > 0 then ", " else "") ++ f countWarnings "warning") ++
                       " " ++ extra ++ [' ' | extra /= ""] ++ "- " ++ takeFileName curdir

            updateTitle $ if isJust test then "(running test) " else ""
            outputFill (Just (loadedCount, messages)) ["Running test..." | isJust test]
            forM_ outputfiles $ \file ->
                writeFile file $ unlines $ map snd $ prettyOutput loadedCount $ filter isMessage messages
            when (null loaded) $ do
                putStrLn $ "No files loaded, nothing to wait for. Fix the last error and restart."
                exitFailure
            whenJust test $ \t -> do
                whenLoud $ outStrLn $ "%TESTING: " ++ t
                sessionExecAsync session t $ do
                    whenLoud $ outStrLn "%TESTING: Completed"
                    updateTitle "(test done)"

            reason <- nextWait $ restart ++ reload ++ loaded
            outputFill Nothing $ "Reloading..." : map ("  " ++) reason
            restartTimes2 <- mapM getModTime restart
            if restartTimes == restartTimes2 then do
                nextWait <- waitFiles waiter
                fire nextWait =<< sessionReload session
            else do
                runGhcid session waiter restart reload command outputfiles test size titles output

    nextWait <- waitFiles waiter
    (messages, loaded) <- sessionStart session command
    when (null loaded) $ do
        putStrLn $ "\nNo files loaded, GHCi is not working properly.\nCommand: " ++ command
        exitFailure
    fire nextWait (messages, loaded)


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: Int -> [Load] -> [(Style,String)]
prettyOutput loaded [] = [(Plain,allGoodMessage ++ " (" ++ show loaded ++ " module" ++ ['s' | loaded /= 1] ++ ")")]
prettyOutput loaded xs = concat $ msg1:msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map (map (Bold,) . loadMessage) err ++ map (map (Plain,) . loadMessage) warn
