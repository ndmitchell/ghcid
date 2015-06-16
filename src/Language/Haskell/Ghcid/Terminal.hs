{-# LANGUAGE CPP #-}
module Language.Haskell.Ghcid.Terminal(
    terminalTopmost,
    withWindowIcon, WindowIcon(..), changeWindowIcon
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


wM_SETICON = 0x0080 :: WindowMessage
wM_GETICON = 0x007F :: WindowMessage

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


data WindowIcon = IconOK | IconWarning | IconError

-- | Change the window icon to green, yellow or red depending on whether the file was errorless, contained only warnings or contained at least one error.
changeWindowIcon :: WindowIcon -> IO ()
#if defined(mingw32_HOST_OS)
changeWindowIcon x = do
    ico <- return $ case x of
        IconOK -> iDI_ASTERISK
        IconWarning -> iDI_EXCLAMATION
        IconError -> iDI_HAND
    icon <- loadIcon Nothing ico
    wnd <- getConsoleWindow
    -- SMALL is the system tray, BIG is the taskbar and Alt-Tab screen
    sendMessage wnd wM_SETICON iCON_SMALL $ fromIntegral $ castPtrToUINTPtr icon
    sendMessage wnd wM_SETICON iCON_BIG $ fromIntegral $ castPtrToUINTPtr icon
    return ()
#else
changeWindowIcon _ = return ()
#endif


withWindowIcon :: IO a -> IO a
#if defined(mingw32_HOST_OS)
withWindowIcon act = do
    wnd <- getConsoleWindow
    icoBig <- sendMessage wnd wM_GETICON iCON_BIG 0
    icoSmall <- sendMessage wnd wM_GETICON iCON_SMALL 0
    act `finally` do
        sendMessage wnd wM_SETICON iCON_BIG icoBig
        sendMessage wnd wM_SETICON iCON_SMALL icoSmall
        return ()
#else
withWindowIcon act = act
#endif
