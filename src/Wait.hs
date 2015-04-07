{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, ScopedTypeVariables, TupleSections #-}

module Wait(Waiter, withWaiterPoll, waitFiles) where

import Control.Exception
import Data.Time.Clock
import System.Console.CmdArgs
import System.Time.Extra
import Language.Haskell.Ghcid.Util


data Waiter = Waiter (IO ([FilePath] -> IO [String]))

-- | Return a message about why you are continuing (usually a file name).
--   Reports any changes between the first
waitFiles :: Waiter -> IO ([FilePath] -> IO [String])
waitFiles (Waiter x) = x


withWaiterPoll :: (Waiter -> IO a) -> IO a
withWaiterPoll f = f $ Waiter $ do
    base <- getCurrentTime
    return $ \files -> handle (\(e :: IOError) -> do sleep 0.1; return [show e]) $ do
        whenLoud $ outStrLn $ "%WAITING: " ++ unwords files
        new <- mapM getModTime files
        case [x | (x,Just t) <- zip files new, t > base] of
            [] -> recheck files new
            xs -> return xs
    where
        recheck files old = do
            sleep 0.1
            new <- mapM getModTime files
            case [x | (x,t1,t2) <- zip3 files old new, t1 /= t2] of
                [] -> recheck files new
                xs -> return xs
