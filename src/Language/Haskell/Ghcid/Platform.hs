{-# LANGUAGE CPP #-}
module Language.Haskell.Ghcid.Platform(
    terminalSize
    ) where

#if !defined(mingw32_HOST_OS)
import qualified System.Console.Terminal.Size as Terminal
import System.IO (stdout)
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
