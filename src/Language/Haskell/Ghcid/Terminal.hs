{-# LANGUAGE CPP #-}
module Language.Haskell.Ghcid.Terminal(
    terminalTopmost,
    changeWindowIcon
    ) where

#if defined(mingw32_HOST_OS)
import Data.Word
import Data.Bits
import Foreign.Ptr

import Graphics.Win32.Misc
import Graphics.Win32.Window 
import Graphics.Win32.Message
import Graphics.Win32.GDI.Types hiding (HWND)
import System.Win32.Types
import Unsafe.Coerce

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


-- | Change the window icon to green, yellow or red depending on whether the file was errorless, contained only warnings or contained at least one error.
changeWindowIcon :: (Ord a, Num a) => a -> a -> IO ()
#if defined(mingw32_HOST_OS)
changeWindowIcon numWarnings numErrors
    | numErrors   > 0   = getIcon iDI_HAND          >>= setIcon
    | numWarnings > 0   = getIcon iDI_EXCLAMATION   >>= setIcon
    | otherwise         = getIcon iDI_ASTERISK      >>= setIcon

    
wM_SETICON :: WindowMessage
wM_SETICON = 0x0080

-- | Wrapper around loadIcon from the win32 library.
getIcon :: Icon -> IO HICON
getIcon = loadIcon Nothing

-- | Sets the icon of the invoking (console) window to the supplied HICON.
setIcon :: HICON -> IO ()
setIcon icon = do
    wnd <- c_GetConsoleWindow
    sendMessage wnd wM_SETICON 0 (unsafeCoerce icon) --the 0 indicates the icon in the screen. unsafeCoerce is used to unwrap several newtype layers (HICON -> HANDLE -> DWORD -> LONG)
    sendMessage wnd wM_SETICON 1 (unsafeCoerce icon) --the 1 indicates the icon in the taskbar and the alt-tab screen
    return () --discard the result of the sendMessage calls
    --only works if the application is not pinned to the taskbar (unlikely for ghcid?)
    
#else
changeWindowIcon _ _ = return ()
#endif