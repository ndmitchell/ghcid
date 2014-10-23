{-# LANGUAGE RecordWildCards, DeriveDataTypeable, CPP, ScopedTypeVariables #-}
-- | The application entry point
module Main(main) where

import System.Console.CmdArgs
import Control.Monad
import Language.Haskell.GhcidProgram
import Language.Haskell.Ghcid.Util



#if defined(mingw32_HOST_OS)
foreign import stdcall unsafe "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO Int

foreign import stdcall unsafe "windows.h SetWindowPos"
    c_SetWindowPos :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int

c_HWND_TOPMOST = -1
#endif

main :: IO ()
main = do
    Options{..} <- cmdArgsRun options
#if defined(mingw32_HOST_OS)
    when topmost $ do
        wnd <- c_GetConsoleWindow
        c_SetWindowPos wnd c_HWND_TOPMOST 0 0 0 0 3
        return ()
#endif
    runGhcid command height (outStr . unlines)
