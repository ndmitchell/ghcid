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
    ,testcommand :: Maybe String
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
    ,testcommand = Nothing &= typ "TESTCOMMAND" &= help "Test command to run after successful typecheck (defaults to not running tests)"
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
            runGhcid waiter restart command testcommand height $ \xs -> do
                outWith $ forM_ (groupOn fst xs) $ \x@((b,_):_) -> do
                    when b $ setSGR [SetConsoleIntensity BoldIntensity]
                    putStr $ concatMap ((:) '\n' . snd) x
                    when b $ setSGR []
                hFlush stdout -- must flush, since we don't finish with a newline


runGhcid :: Waiter -> [FilePath] -> String -> Maybe String -> IO (Int,Int) -> ([(Bool,String)] -> IO ()) -> IO ()
runGhcid waiter restart command testcommand size output = do
    restartTimes <- mapM getModTime restart
    do (_,height) <- size; output $ map (False,) $ "Loading..." : replicate (height - 1) ""
    (ghci,initLoad) <- startGhci command Nothing
    curdir <- getCurrentDirectory
    let fire load warnings = do
            load <- return $ filter (not . whitelist) load
            waitFiles <- waitFiles waiter
            modsActive <- fmap (map snd) $ showModules ghci
            let modsLoad = nubOrd $ map loadFile load
            whenLoud $ do
                outStrLn $ "%ACTIVE: " ++ show modsActive
                outStrLn $ "%LOAD: " ++ show load
            let warn = [w | w <- warnings, loadFile w `elem` modsActive, loadFile w `notElem` modsLoad]
            (width, height) <- size
            let outFill msg = output $ take height $ msg ++ map (False,) (replicate height "")
            let errorsListing = [m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m} | m@Message{} <- load ++ warn]
            outFill $ prettyOutput height
                [m{loadMessage = concatMap (chunksOfWord width (width `div` 5)) $ loadMessage m} | m@Message{} <- load ++ warn]

            case testcommand of
              Just cmd -> do
                if length errorsListing == 0
                then runTests cmd ghci >>= putStrLn . unlines
                else return ()
              Nothing -> return ()

            setTitle $
                let (errs, warns) = both sum $ unzip [if loadSeverity m == Error then (1,0) else (0,1) | m@Message{} <- load ++ warn]
                    f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                in (if errs == 0 && warns == 0 then allGoodMessage else f errs "error" ++
                    (if errs > 0 && warns > 0 then ", " else "") ++ f warns "warning") ++
                   " - " ++ takeFileName curdir
            let wait = nubOrd $ modsLoad ++ modsActive
            when (null wait) $ do
                putStrLn $ "No files loaded, probably did not start GHCi.\nCommand: " ++ command
                exitFailure
            reason <- waitFiles $ restart ++ wait
            outFill $ map (False,) $ "Reloading..." : map ("  " ++) reason
            restartTimes2 <- mapM getModTime restart
            if restartTimes == restartTimes2 then do
                load2 <- reload ghci
                fire load2 [m | m@Message{..} <- warn ++ load, loadSeverity == Warning]
            else do
                stopGhci ghci
                runGhcid waiter restart command testcommand size output
    fire initLoad []


whitelist :: Load -> Bool
whitelist Message{loadSeverity=Warning, loadMessage=[_,x]}
    = x `elem` ["    -O conflicts with --interactive; -O ignored."]
whitelist _ = False


prettyOutput :: Int -> [Load] -> [(Bool,String)]
prettyOutput _ [] = [(False,allGoodMessage)]
prettyOutput height xs = take (max 3 $ height - (length msgs * 2)) msg1 ++ concatMap (take 2) msgs
    where (err, warn) = partition ((==) Error . loadSeverity) xs
          msg1:msgs = map (map (True,) . loadMessage) err ++ map (map (False,) . loadMessage) warn
