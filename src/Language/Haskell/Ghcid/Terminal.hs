{-# LANGUAGE CPP #-}
module Language.Haskell.Ghcid.Terminal(
    terminalSize, terminalTopmost
    ) where

#if !defined(mingw32_HOST_OS)
import qualified System.Console.Terminal.Size as Terminal
import System.IO (stdout)
import Data.Tuple.Extra
#endif

#if defined(mingw32_HOST_OS)
import Control.Monad
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

type HANDLE = Ptr ()
type HWND = Ptr ()

data CONSOLE_SCREEN_BUFFER_INFO
sizeCONSOLE_SCREEN_BUFFER_INFO = 22
posCONSOLE_SCREEN_BUFFER_INFO_srWindow = 10 -- 4 x Word16 Left,Top,Right,Bottom

c_SWP_NOSIZE = 1 :: Word32
c_SWP_NOMOVE = 2 :: Word32
c_HWND_TOPMOST = -1 :: Int
c_STD_OUTPUT_HANDLE = -11 :: Word32

foreign import stdcall unsafe "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO HWND

foreign import stdcall unsafe "windows.h SetWindowPos"
    c_SetWindowPos :: HWND -> Int -> Int -> Int -> Int -> Int -> Word32 -> IO Bool

foreign import stdcall unsafe "windows.h GetConsoleScreenBufferInfo"
    c_GetConsoleScreenBufferInfo :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFO -> IO Bool

foreign import stdcall unsafe "windows.h GetStdHandle"
    c_GetStdHandle :: Word32 -> IO HANDLE
#endif


-- | Figure out the size of the current terminal, @(width,height)@, or return 'Nothing'.
terminalSize :: IO (Maybe (Int, Int))
#if defined(mingw32_HOST_OS)
terminalSize = do
    hdl <- c_GetStdHandle c_STD_OUTPUT_HANDLE
    allocaBytes sizeCONSOLE_SCREEN_BUFFER_INFO $ \p -> do
        b <- c_GetConsoleScreenBufferInfo hdl p
        if not b then return Nothing else do
            [left,top,right,bottom] <- forM [0..3] $ \i -> do
                v <- peekByteOff p ((i*2) + posCONSOLE_SCREEN_BUFFER_INFO_srWindow)
                return $ fromIntegral (v :: Word16)
            return $ Just (1+right-left, 1+bottom-top)
#else
terminalSize = do
    s <- Terminal.hSize stdout
    return $ fmap (Terminal.width &&& Terminal.height) s
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
