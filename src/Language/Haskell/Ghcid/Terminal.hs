{-# LANGUAGE CPP #-}
module Language.Haskell.Ghcid.Terminal(
    terminalTopmost
    ) where

#if defined(mingw32_HOST_OS)
import Data.Word
import Data.Bits
import Foreign.Ptr

type HWND = Ptr ()

c_SWP_NOSIZE = 1 :: Word32
c_SWP_NOMOVE = 2 :: Word32
c_HWND_TOPMOST = -1 :: Int

foreign import stdcall unsafe "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO HWND

foreign import stdcall unsafe "windows.h SetWindowPos"
    c_SetWindowPos :: HWND -> Int -> Int -> Int -> Int -> Int -> Word32 -> IO Bool
#endif


-- | Raise the current terminal on top of all other screens, if you can.
terminalTopmost :: IO ()
#if defined(mingw32_HOST_OS)
terminalTopmost = do
    wnd <- c_GetConsoleWindow
    c_SetWindowPos wnd c_HWND_TOPMOST 0 0 0 0 (c_SWP_NOMOVE .|. c_SWP_NOSIZE)
    return ()
#else
terminalTopmost = return ()
#endif
