
module GHCi(
    ghci,
    parseShowModules,
    Severity(..), Load(..), isMessage, parseLoad
    ) where

import System.IO
import System.Process
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.FilePath
import System.Console.CmdArgs.Verbosity
import Util


---------------------------------------------------------------------
-- IO INTERACTION WITH GHCI

ghci :: String -> IO (String -> IO [String])
ghci cmd = do
    (inp, out, err, pid) <- runInteractiveProcess cmd [] Nothing Nothing
    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    lock <- newMVar () -- ensure only one person talks to ghci at a time
    outs <- newMVar [] -- result that is buffering up
    errs <- newMVar []
    flush <- newEmptyMVar -- result is moved to push once we see separate

    let prefix = "#~GHCID-PREFIX~#"
    let separate = "#~GHCID-SEPARATE~#"
    hPutStrLn inp $ ":set prompt " ++ prefix

    forM_ [(out,outs,"GHCOUT"),(err,errs,"GHCERR")] $ \(h,buf,strm) -> forkIO $ forever $ do
        s <- hGetLine h
        whenLoud $ outStrLn $ "%" ++ strm ++ ": " ++ s
        if s == separate then do
            outs <- modifyMVar outs $ \s -> return ([], reverse s)
            errs <- modifyMVar errs $ \s -> return ([], reverse s)
            putMVar flush $ outs ++ errs
        else
            modifyMVar_ buf $ return . (fromMaybe s (stripPrefix prefix s):)

    return $ \s -> withMVar lock $ const $ do
        whenLoud $ outStrLn $ "%GHCINP: " ++ s
        hPutStrLn inp $ s ++ "\nputStrLn \"\\n" ++ separate ++ "\""
        res <- takeMVar flush
        return res


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
