{-# LANGUAGE CPP #-}
module Language.Haskell.Ghcid.Terminal(
    terminalSize, terminalTopmost,
    ) where

#if !defined(mingw32_HOST_OS)
import qualified System.Console.Terminal.Size as Terminal
import System.IO (stdout)
#endif

#if defined(mingw32_HOST_OS)
foreign import stdcall unsafe "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO Int

foreign import stdcall unsafe "windows.h SetWindowPos"
    c_SetWindowPos :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int

c_HWND_TOPMOST = -1
#endif


-- | Figure out the size of the current terminal, width\/height, or return 'Nothing'.
terminalSize :: IO (Maybe (Int, Int))
#if defined(mingw32_HOST_OS)
terminalSize = return Nothing
#else
terminalSize = do
    s <- Terminal.hSize stdout
    return $ case s of
        Just w -> return $ Just (Terminal.width w, Terminal.height w)
        Nothing -> return Nothing
#endif


-- | Raise the current terminal on top of all other screens, if you can.
terminalTopmost :: IO ()
terminalTopmost = do
    wnd <- c_GetConsoleWindow
    c_SetWindowPos wnd c_HWND_TOPMOST 0 0 0 0 3
    return ()
