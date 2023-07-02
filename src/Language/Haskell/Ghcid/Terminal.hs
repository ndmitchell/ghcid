{-# LANGUAGE CPP #-}

-- | Cross-platform operations for manipulating terminal console windows.
module Language.Haskell.Ghcid.Terminal(
    terminalTopmost,
    withWindowIcon, WindowIcon(..), setWindowIcon
    ) where

#if defined(mingw32_HOST_OS)
import Data.Word
import Data.Bits
import Control.Exception

import Graphics.Win32.Misc
import Graphics.Win32.Window
import Graphics.Win32.Message
import Graphics.Win32.GDI.Types
import System.Win32.Types


wM_GETICON = 0x007F :: WindowMessage

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "windows.h GetConsoleWindow"
    getConsoleWindow :: IO HWND

foreign import CALLCONV unsafe "windows.h SetWindowPos"
    setWindowPos :: HWND -> HWND -> Int -> Int -> Int -> Int -> Word32 -> IO Bool
#endif


-- | Raise the current terminal on top of all other screens, if you can.
terminalTopmost :: IO ()
#if defined(mingw32_HOST_OS)
terminalTopmost = do
    wnd <- getConsoleWindow
    setWindowPos wnd hWND_TOPMOST 0 0 0 0 (sWP_NOMOVE .|. sWP_NOSIZE)
    pure ()
#else
terminalTopmost = pure ()
#endif


data WindowIcon = IconOK | IconWarning | IconError

-- | Change the window icon to green, yellow or red depending on whether the file was errorless, contained only warnings or contained at least one error.
setWindowIcon :: WindowIcon -> IO ()
#if defined(mingw32_HOST_OS)
setWindowIcon x = do
    ico <- pure $ case x of
        IconOK -> iDI_ASTERISK
        IconWarning -> iDI_EXCLAMATION
        IconError -> iDI_HAND
    icon <- loadIcon Nothing ico
    wnd <- getConsoleWindow
    -- SMALL is the system tray, BIG is the taskbar and Alt-Tab screen
    sendMessage wnd wM_SETICON iCON_SMALL $ fromIntegral $ castPtrToUINTPtr icon
    sendMessage wnd wM_SETICON iCON_BIG $ fromIntegral $ castPtrToUINTPtr icon
    pure ()
#else
setWindowIcon _ = pure ()
#endif


-- | Run an operation in which you call setWindowIcon
withWindowIcon :: IO a -> IO a
#if defined(mingw32_HOST_OS)
withWindowIcon act = do
    wnd <- getConsoleWindow
    icoBig <- sendMessage wnd wM_GETICON iCON_BIG 0
    icoSmall <- sendMessage wnd wM_GETICON iCON_SMALL 0
    act `finally` do
        sendMessage wnd wM_SETICON iCON_BIG icoBig
        sendMessage wnd wM_SETICON iCON_SMALL icoSmall
        pure ()
#else
withWindowIcon act = act
#endif
