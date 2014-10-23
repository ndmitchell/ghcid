{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, ScopedTypeVariables #-}
-- | The application entry point
module Main(main) where

import System.Console.CmdArgs
import Control.Monad
import Language.Haskell.GhcidProgram
import Language.Haskell.Ghcid.Terminal
import Language.Haskell.Ghcid.Util


main :: IO ()
main = do
    Options{..} <- cmdArgsRun options
    when topmost terminalTopmost
    runGhcid command height (outStr . unlines)
