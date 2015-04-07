{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, ScopedTypeVariables, TupleSections #-}

module Wait(waitFiles) where

import Control.Exception
import Data.Time.Clock
import System.Console.CmdArgs
import System.Time.Extra

import Language.Haskell.Ghcid.Util



-- | return a message about why you are continuing (usually a file name)
waitFiles :: UTCTime -> [FilePath] -> IO [String]
waitFiles base files = handle (\(e :: IOError) -> do sleep 0.1; return [show e]) $ do
    whenLoud $ outStrLn $ "%WAITING: " ++ unwords files
    new <- mapM getModTime files
    case [x | (x,Just t) <- zip files new, t > base] of
        [] -> recheck files new
        xs -> return xs
    where
        recheck files' old = do
            sleep 0.1
            new <- mapM getModTime files'
            case [x | (x,t1,t2) <- zip3 files' old new, t1 /= t2] of
                [] -> recheck files' new
                xs -> return xs
