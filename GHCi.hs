
module GHCi(
    ghci,
    parseShowModules,
    Severity(..), Load(..), parseLoad
    ) where

import System.IO
import System.Process
import Control.Concurrent
import Control.Monad
import Data.Char


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

-- *Main> :show modules
-- Main             ( Main.hs, interpreted )
-- GHCi             ( GHCi.hs, interpreted )
parseShowModules :: [String] -> [(String, FilePath)]
parseShowModules xs =
    [ (takeWhile (not . isSpace) a, takeWhile (/= ',') b)
    | x <- xs, (a,'(':' ':b) <- [break (== '(') x]]

data Severity = Warning | Error

data Load = Load 
    {loadSeverity :: Severity
    ,loadFile :: FilePath
    ,loadFilePos :: (Int,Int)
    ,loadMessage :: [String]
    }

parseLoad :: [String] -> [String]
parseLoad = id
