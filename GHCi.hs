
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
import System.FilePath


---------------------------------------------------------------------
-- IO INTERACTION WITH GHCI

ghci :: IO (String -> IO [String])
ghci = do
    (inp, out, err, pid) <- runInteractiveProcess "ghci" [] Nothing Nothing
    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    lock <- newMVar () -- ensure only one person talks to ghci at a time
    outs <- newMVar [] -- result that is buffering up
    errs <- newMVar []
    flush <- newEmptyMVar -- result is moved to push once we see magic

    -- me <- newMVar ()

    let magic = "#GHCID#"
    forM_ [(out,outs),(err,errs)] $ \(h,buf) -> forkIO $ forever $ do
        s <- hGetLine h
        --withMVar me $ const $ putStrLn $ "%OUTPUT: " ++ s
        if s == magic then do
            outs <- modifyMVar outs $ \s -> return ([], reverse s)
            errs <- modifyMVar errs $ \s -> return ([], reverse s)
            putMVar flush $ outs ++ errs
        else
            modifyMVar_ buf $ return . (s:)

    return $ \s -> withMVar lock $ const $ do
        --withMVar me $ const $ putStrLn $ "%INPUT: " ++ s
        hPutStrLn inp $ s ++ "\nputStrLn $ \"\\n\" ++ " ++ show magic
        takeMVar flush



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
    = Message sev file (p1,p2) (dropWhile null $ rest:msg) : parseLoad xs
parseLoad (x:xs) = parseLoad xs
parseLoad [] = []
