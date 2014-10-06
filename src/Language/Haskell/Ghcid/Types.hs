{-# LANGUAGE DeriveDataTypeable #-}
-- | The types we use
-- Copyright Neil Mitchell 2014.
module Language.Haskell.Ghcid.Types where

import Data.Typeable
import Control.Exception.Base (Exception)

-- | Send a command, get lines of result
type GhciExec = String -> IO [String]

-- | GHCi shut down
data GhciError = UnexpectedExit String String
  deriving (Show,Eq,Ord,Typeable)

-- | Make GhciError an exception
instance Exception GhciError

-- | Severity of messages
data Severity = Warning | Error 
  deriving (Show,Eq,Ord,Bounded,Enum,Typeable)

-- | Load messages
data Load
    = Loading {loadModule :: String, loadFile :: FilePath}
    | Message
        {loadSeverity :: Severity
        ,loadFile :: FilePath
        ,loadFilePos :: (Int,Int)
        ,loadMessage :: [String]
        }
      deriving (Show,Eq)
      
-- | Is a Load a message with severity?
isMessage :: Load -> Bool
isMessage Message{} = True
isMessage _ = False