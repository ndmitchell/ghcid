{-# LANGUAGE DeriveDataTypeable #-}
-- | The types we use
-- Copyright Neil Mitchell 2014. See <https://github.com/ndmitchell/ghcid>
module Language.Haskell.Ghcid.Types where

import Data.Typeable
import Control.Exception.Base (Exception)

type GhciExec = String -> IO [String]

data GhciError = UnexpectedExit String String
  deriving (Show,Eq,Ord,Typeable)

instance Exception GhciError

data Severity = Warning | Error 
  deriving (Show,Eq,Ord,Bounded,Enum,Typeable)


data Load
    = Loading {loadModule :: String, loadFile :: FilePath}
    | Message
        {loadSeverity :: Severity
        ,loadFile :: FilePath
        ,loadFilePos :: (Int,Int)
        ,loadMessage :: [String]
        }
      deriving (Show,Eq)
      

isMessage :: Load -> Bool
isMessage Message{} = True
isMessage _ = False