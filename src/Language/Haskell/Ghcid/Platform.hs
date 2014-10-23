{-# LANGUAGE CPP #-}
module Language.Haskell.Ghcid.Platform
  ( terminalSize
  )
  where

#if !defined(mingw32_HOST_OS)
import qualified System.Console.Terminal.Size as Terminal
import System.IO (stdout)
#endif

terminalSize :: IO (Int, Int)

#if defined(mingw32_HOST_OS)

terminalSize = error "Couldn't determine terminal window size"

#else

terminalSize = do
  s <- Terminal.hSize stdout
  case s of
    Just w -> return (Terminal.width w, Terminal.height w)
    Nothing -> error "Couldn't determine terminal window size"

#endif
