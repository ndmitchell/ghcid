{-# LANGUAGE CPP #-}
module Language.Haskell.Ghcid.Terminal(
    terminalTopmost,
    changeWindowIcon
    ) where

#if defined(mingw32_HOST_OS)
import Data.Word
import Data.Bits

import Graphics.Win32.Misc
import Graphics.Win32.Window 
import Graphics.Win32.Message
import Graphics.Win32.GDI.Types
import Unsafe.Coerce

wM_SETICON = 0x0080 :: WindowMessage
iCON_BIG = 1
iCON_SMALL = 0


foreign import stdcall unsafe "windows.h GetConsoleWindow"
    getConsoleWindow :: IO HWND

foreign import stdcall unsafe "windows.h SetWindowPos"
    setWindowPos :: HWND -> HWND -> Int -> Int -> Int -> Int -> Word32 -> IO Bool
#endif


-- | Raise the current terminal on top of all other screens, if you can.
terminalTopmost :: IO ()
#if defined(mingw32_HOST_OS)
terminalTopmost = do
    wnd <- getConsoleWindow
    setWindowPos wnd hWND_TOPMOST 0 0 0 0 (sWP_NOMOVE .|. sWP_NOSIZE)
    return ()
#else
terminalTopmost = return ()
#endif


-- | Change the window icon to green, yellow or red depending on whether the file was errorless, contained only warnings or contained at least one error.
changeWindowIcon :: (Ord a, Num a) => a -> a -> IO ()
#if defined(mingw32_HOST_OS)
changeWindowIcon numWarnings numErrors
    | numErrors   > 0   = f iDI_HAND
    | numWarnings > 0   = f iDI_EXCLAMATION
    | otherwise         = f iDI_ASTERISK
    where
        f ico = do
            icon <- loadIcon Nothing ico
            wnd <- getConsoleWindow
            sendMessage wnd wM_SETICON iCON_SMALL (unsafeCoerce icon) --the 0 indicates the icon in the screen. unsafeCoerce is used to unwrap several newtype layers (HICON -> HANDLE -> DWORD -> LONG)
            sendMessage wnd wM_SETICON iCON_BIG (unsafeCoerce icon) --the 1 indicates the icon in the taskbar and the alt-tab screen
            return ()
#else
changeWindowIcon _ _ = return ()
#endif
