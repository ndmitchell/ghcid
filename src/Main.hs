{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, ScopedTypeVariables #-}
-- | The application entry point
module Main(main) where

import System.Console.CmdArgs
import Control.Monad
import Language.Haskell.GhcidProgram
import Language.Haskell.Ghcid.Terminal
import Language.Haskell.Ghcid.Util

-- | Command line options
data Options = Options
    {command :: String
    ,height :: Int
    ,topmost :: Bool
    }
    deriving (Data,Typeable,Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {command = "" &= typ "COMMAND" &= help "Command to run (defaults to ghci or cabal repl)"
    ,height = 8 &= help "Number of lines to show"
    ,topmost = False &= name "t" &= help "Set window topmost (Windows only)"
    } &= verbosity &=
    program "ghcid" &= summary "Auto :reload'ing GHCi daemon"


main :: IO ()
main = do
    Options{..} <- cmdArgsRun options
    when topmost terminalTopmost
    runGhcid command height (outStr . unlines)
