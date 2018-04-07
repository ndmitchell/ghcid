{-# LANGUAGE RecordWildCards, DeriveDataTypeable, TupleSections #-}
{-# OPTIONS_GHC -fno-cse #-}

-- | The application entry point
module Ghcid(main, mainWithTerminal) where

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
import System.Console.CmdArgs.Explicit
import System.Console.ANSI
import System.Environment
import System.Directory.Extra
import System.Time.Extra
import System.Exit
import System.FilePath
import System.IO.Extra

import Paths_ghcid
import Language.Haskell.Ghcid.Terminal
import Language.Haskell.Ghcid.Util
import Language.Haskell.Ghcid.Types
import Wait

import Data.Functor
import Prelude


-- | Command line options
data Options = Options
    {command :: String
    ,arguments :: [String]
    ,test :: [String]
    ,warnings :: Bool
    ,no_status :: Bool
    ,height :: Maybe Int
    ,width :: Maybe Int
    ,topmost :: Bool
    ,no_title :: Bool
    ,project :: String
    ,reload :: [FilePath]
    ,restart :: [FilePath]
    ,directory :: FilePath
    ,outputfile :: [FilePath]
    ,ignoreLoaded :: Bool
    ,poll :: Maybe Seconds
    ,max_messages :: Maybe Int
    ,color :: ColorMode
    }
    deriving (Data,Typeable,Show)

-- | When to colour terminal output.
data ColorMode
    = Never  -- ^ Terminal output will never be coloured.
    | Always -- ^ Terminal output will always be coloured.
    | Auto   -- ^ Terminal output will be coloured if $TERM and stdout appear to support it.
      deriving (Show, Typeable, Data)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= name "c" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,arguments = [] &= args &= typ "MODULE"
    ,test = [] &= name "T" &= typ "EXPR" &= help "Command to run after successful loading"
    ,warnings = False &= name "W" &= help "Allow tests to run even with warnings"
    ,no_status = False &= name "S" &= help "Suppress status messages"
    ,height = Nothing &= help "Number of lines to use (defaults to console height)"
    ,width = Nothing &= name "w" &= help "Number of columns to use (defaults to console width)"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    ,no_title = False &= help "Don't update the shell title/icon"
    ,project = "" &= typ "NAME" &= help "Name of the project, defaults to current directory"
    ,restart = [] &= typ "PATH" &= help "Restart the command when the given file or directory contents change (defaults to .ghci and any .cabal file)"
    ,reload = [] &= typ "PATH" &= help "Reload when the given file or directory contents change (defaults to none)"
    ,directory = "." &= typDir &= name "C" &= help "Set the current directory"
    ,outputfile = [] &= typFile &= name "o" &= help "File to write the full output to"
    ,ignoreLoaded = False &= explicit &= name "ignore-loaded" &= help "Keep going if no files are loaded. Requires --reload to be set."
    ,poll = Nothing &= typ "SECONDS" &= opt "0.1" &= explicit &= name "poll" &= help "Use polling every N seconds (defaults to using notifiers)"
    ,max_messages = Nothing &= name "n" &= help "Maximum number of messages to print"
    ,color = Auto &= name "colour" &= name "color" &= opt Always &= typ "always/never/auto" &= help "Color output (defaults to when the terminal supports it)"
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
        let isStack dir = flip catchIOError (const $ return False) $
                doesFileExist (dir </> "stack.yaml") &&^ doesDirectoryExist (dir </> ".stack-work")
        stack <- isStack "." ||^ isStack ".." -- stack file might be parent, see #62

        let cabal = filter ((==) ".cabal" . takeExtension) files
        let opts = ["-fno-code" | null test]
        return $ case () of
            _ | stack ->
                let flags = if null arguments then
                                "stack ghci --test" :
                                ["--no-load" | ".ghci" `elem` files] ++
                                map ("--ghci-options=" ++) opts
                            else
                                "stack exec --test -- ghci" : opts
                in f flags $ "stack.yaml":cabal
              | ".ghci" `elem` files -> f ("ghci":opts) [".ghci"]
              | cabal /= [] -> f (if null arguments then "cabal repl":map ("--ghc-options=" ++) opts else "cabal exec -- ghci":opts) cabal
              | otherwise -> f ("ghci":opts) []
    where
        f c r = o{command = unwords $ c ++ map escape arguments, arguments = [], restart = restart ++ r}

        -- in practice we're not expecting many arguments to have anything funky in them
        escape x | ' ' `elem` x = "\"" ++ x ++ "\""
                 | otherwise = x

-- | Use arguments from .ghcid if present
withGhcidArgs :: IO a -> IO a
withGhcidArgs act = do
    b <- doesFileExist ".ghcid"
    if not b then act else do
        extra <- concatMap splitArgs . lines <$> readFile' ".ghcid"
        orig <- getArgs
        withArgs (extra ++ orig) act


-- | Like 'main', but run with a fake terminal for testing
mainWithTerminal :: IO (Int,Int) -> ([(Style,String)] -> IO ()) -> IO ()
mainWithTerminal termSize termOutput = withWindowIcon $ withSession $ \session -> do
    -- On certain Cygwin terminals stdout defaults to BlockBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr NoBuffering
    opts <- withGhcidArgs $ cmdArgsRun options
    withCurrentDirectory (directory opts) $ do
        opts <- autoOptions opts
        opts <- return $ opts{restart = nubOrd $ restart opts, reload = nubOrd $ reload opts}
        when (topmost opts) terminalTopmost

        termSize <- return $ case (width opts, height opts) of
            (Just w, Just h) -> return (w,h)
            (w, h) -> do
                term <- termSize
                -- if we write to the final column of the window then it wraps automatically
                -- so putStrLn width 'x' uses up two lines
                return (fromMaybe (pred $ fst term) w, fromMaybe (snd term) h)

        restyle <- do
            useStyle <- case color opts of
                Always -> return True
                Never -> return False
                Auto -> hSupportsANSI stdout
            return $ if useStyle then id else map (first $ const Plain)

        maybe withWaiterNotify withWaiterPoll (poll opts) $ \waiter ->
            handle (\(UnexpectedExit cmd _) -> putStrLn $ "Command \"" ++ cmd ++ "\" exited unexpectedly") $
                runGhcid session waiter termSize (termOutput . restyle) opts



main :: IO ()
main = mainWithTerminal termSize termOutput
    where
        termSize = maybe (80, 8) (Term.width &&& Term.height) <$> Term.size

        termOutput xs = do
            outWith $ forM_ (groupOn fst xs) $ \x@((s,_):_) -> do
                when (s == Bold) $ setSGR [SetConsoleIntensity BoldIntensity]
                putStr $ concatMap ((:) '\n' . snd) x
                when (s == Bold) $ setSGR []
            hFlush stdout -- must flush, since we don't finish with a newline


data Style = Plain | Bold deriving Eq


runGhcid :: Session -> Waiter -> IO (Int,Int) -> ([(Style,String)] -> IO ()) -> Options -> IO ()
runGhcid session waiter termSize termOutput opts@Options{..} = do
    let outputFill :: Maybe (Int, [Load]) -> [String] -> IO ()
        outputFill load msg = do
            (width, height) <- termSize
            let n = height - length msg
            currTime <- getShortTime
            load <- return $ take (if isJust load then n else 0) $ prettyOutput max_messages currTime (maybe 0 fst load)
                [ m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m}
                | m@Message{} <- maybe [] snd load]
            termOutput $ load ++ map (Plain,) msg ++ replicate (height - (length load + length msg)) (Plain,"")

    when (ignoreLoaded && null reload) $ do
        putStrLn "--reload must be set when using --ignore-loaded"
        exitFailure

    restartTimes <- mapM getModTime restart
    curdir <- getCurrentDirectory
    currTime <- getShortTime

    -- fire, given a waiter, the messages/loaded
    let fire nextWait (messages, loaded) = do
            let loadedCount = length loaded
            whenLoud $ do
                outStrLn $ "%MESSAGES: " ++ show messages
                outStrLn $ "%LOADED: " ++ show loaded

            let (countErrors, countWarnings) = both sum $ unzip
                    [if loadSeverity == Error then (1,0) else (0,1) | m@Message{..} <- messages, loadMessage /= []]
            test <- return $
                if null test || countErrors /= 0 || (countWarnings /= 0 && not warnings) then Nothing
                else Just $ intercalate "\n" test

            unless no_title $ setWindowIcon $
                if countErrors > 0 then IconError else if countWarnings > 0 then IconWarning else IconOK

            let updateTitle extra = unless no_title $ setTitle $
                    let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                    in (if countErrors == 0 && countWarnings == 0 then allGoodMessage ++ ", at " ++ currTime else f countErrors "error" ++
                       (if countErrors >  0 && countWarnings >  0 then ", " else "") ++ f countWarnings "warning") ++
                       " " ++ extra ++ [' ' | extra /= ""] ++ "- " ++
                       (if null project then takeFileName curdir else project)

            updateTitle $ if isJust test then "(running test)" else ""
            outputFill (Just (loadedCount, messages)) ["Running test..." | isJust test]
            forM_ outputfile $ \file ->
                writeFile file $ unlines $ map snd $ prettyOutput max_messages currTime loadedCount $ filter isMessage messages
            when (null loaded && not ignoreLoaded) $ do
                putStrLn "No files loaded, nothing to wait for. Fix the last error and restart."
                exitFailure
            whenJust test $ \t -> do
                whenLoud $ outStrLn $ "%TESTING: " ++ t
                sessionExecAsync session t $ \stderr -> do
                    whenLoud $ outStrLn "%TESTING: Completed"
                    hFlush stdout -- may not have been a terminating newline from test output
                    if "*** Exception: " `isPrefixOf` stderr then do
                        updateTitle "(test failed)"
                        setWindowIcon IconError
                     else do
                        updateTitle "(test done)"
                        whenNormal $ outStrLn "\n...done"

            reason <- nextWait $ restart ++ reload ++ loaded
            unless no_status $ outputFill Nothing $ "Reloading..." : map ("  " ++) reason
            restartTimes2 <- mapM getModTime restart
            if restartTimes == restartTimes2 then do
                nextWait <- waitFiles waiter
                fire nextWait =<< sessionReload session
            else
                runGhcid session waiter termSize termOutput opts

    nextWait <- waitFiles waiter
    (messages, loaded) <- sessionStart session command
    when (null loaded && not ignoreLoaded) $ do
        putStrLn $ "\nNo files loaded, GHCi is not working properly.\nCommand: " ++ command
        exitFailure
    fire nextWait (messages, loaded)


-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: Maybe Int -> String -> Int -> [Load] -> [(Style,String)]
prettyOutput _ currTime loaded [] = [(Plain,allGoodMessage ++ " (" ++ show loaded ++ " module" ++ ['s' | loaded /= 1] ++ ", at " ++ currTime ++ ")")]
prettyOutput maxMsgs _ loaded xs = concat $ maybe id take maxMsgs (msg1:msgs)
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map (map (Bold,) . loadMessage) err ++ map (map (Plain,) . loadMessage) warn
