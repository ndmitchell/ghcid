{-# LANGUAGE ScopedTypeVariables #-}

module GHCi(
    ghci,
    parseShowModules,
    Severity(..), Load(..), isMessage, parseLoad
    ) where

import System.IO
import System.Process
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.FilePath
import System.Console.CmdArgs.Verbosity
import System.Exit
import Util


---------------------------------------------------------------------
-- IO INTERACTION WITH GHCI

ghci :: String -> IO (String -> IO [String])
ghci cmd = do
    (Just inp, Just out, Just err, pid) <-
        createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe}
    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    lock <- newMVar () -- ensure only one person talks to ghci at a time
    let prefix = "#~GHCID-IGNORE~#"
    let finish = "#~GHCID-FINISH~#"
    hPutStrLn inp $ ":set prompt " ++ prefix

    -- consume from a handle, produce an MVar with either Just and a message, or Nothing (stream closed)
    let consume h name = do
            result <- newEmptyMVar -- the end result
            buffer <- newMVar [] -- the things to go in result
            forkIO $ fix $ \rec -> do
                s <- try $ hGetLine h
                case s of
                    Left (_ :: SomeException) -> putMVar result Nothing
                    Right s -> do
                        whenLoud $ outStrLn $ "%" ++ name ++ ": " ++ s
                        if finish `isInfixOf` s then do
                            buf <- modifyMVar buffer $ \old -> return ([], reverse old)
                            putMVar result $ Just buf
                        else
                            modifyMVar_ buffer $ return . (dropPrefix prefix s:)
                        rec
            return result

    outs <- consume out "GHCOUT"
    errs <- consume err "GHCERR"
    firstTime <- newMVar True

    return $ \s -> withMVar lock $ const $ do
        whenLoud $ outStrLn $ "%GHCINP: " ++ s
        hPutStrLn inp $ s ++ "\nputStrLn " ++ show finish ++ "\nerror " ++ show finish
        out <- takeMVar outs
        err <- takeMVar errs
        case liftM2 (++) out err of
            Nothing -> do
                firstTime <- readMVar firstTime
                outStrLn $ if firstTime
                    then "GHCi exited, did you run the right command? You ran:\n  " ++ cmd
                    else "GHCi exited"
                exitFailure
            Just msg -> do
                modifyMVar_ firstTime $ const $ return False
                return msg


dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix pre s = maybe s (dropPrefix pre) $ stripPrefix pre s

---------------------------------------------------------------------
-- PARSING THE OUTPUT

-- Main             ( Main.hs, interpreted )
-- GHCi             ( GHCi.hs, interpreted )
parseShowModules :: [String] -> [(String, FilePath)]
parseShowModules xs =
    [ (takeWhile (not . isSpace) $ dropWhile isSpace a, takeWhile (/= ',') b)
    | x <- xs, (a,'(':' ':b) <- [break (== '(') x]]

data Severity = Warning | Error deriving (Show,Eq)

data Load
    = Loading {loadModule :: String, loadFile :: FilePath}
    | Message
        {loadSeverity :: Severity
        ,loadFile :: FilePath
        ,loadFilePos :: (Int,Int)
        ,loadMessage :: [String]
        }
      deriving Show

isMessage Message{} = True; isMessage _ = False

-- [1 of 2] Compiling GHCi             ( GHCi.hs, interpreted )
-- GHCi.hs:70:1: Parse error: naked expression at top level
-- GHCi.hs:72:13:
--     No instance for (Num ([String] -> [String]))
--       arising from the literal `1'
--     Possible fix:
--       add an instance declaration for (Num ([String] -> [String]))
--     In the expression: 1
--     In an equation for `parseLoad': parseLoad = 1
-- GHCi.hs:81:1: Warning: Defined but not used: `foo'
parseLoad :: [String] -> [Load]
parseLoad (('[':xs):rest) =
    map (uncurry Loading) (parseShowModules [drop 11 $ dropWhile (/= ']') xs]) ++
    parseLoad rest
parseLoad (x:xs)
    | not $ " " `isPrefixOf` x
    , (file,':':rest) <- break (== ':') x
    , takeExtension file `elem` [".hs",".lhs"]
    , (pos,rest) <- span (\x -> x == ':' || isDigit x) rest
    , [p1,p2] <- map read $ words $ map (\x -> if x == ':' then ' ' else x) pos 
    , (msg,xs) <- span (isPrefixOf " ") xs
    , rest <- dropWhile isSpace rest
    , sev <- if "Warning:" `isPrefixOf` rest then Warning else Error
    = Message sev file (p1,p2) (x:msg) : parseLoad xs
parseLoad (x:xs) = parseLoad xs
parseLoad [] = []
