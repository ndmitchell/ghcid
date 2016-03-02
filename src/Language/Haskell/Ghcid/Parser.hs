{-# LANGUAGE PatternGuards #-}
-- | Parses the output from GHCi
module Language.Haskell.Ghcid.Parser
  ( parseShowModules
  , parseLoad
  )
  where

import System.FilePath
import Data.Char
import Data.List.Extra

import Language.Haskell.Ghcid.Types


-- | Parse messages from show modules command
parseShowModules :: [String] -> [(String, FilePath)]
parseShowModules xs =
    [ (takeWhile (not . isSpace) $ trimStart a, takeWhile (/= ',') b)
    | x <- xs, (a,'(':' ':b) <- [break (== '(') x]]

-- | Parse messages given on reload
-- nub, because cabal repl sometimes does two reloads at the start
parseLoad :: [String] -> [Load]
parseLoad  = nubOrd . parseLoad'

-- | Parse messages given on reload
parseLoad' :: [String] -> [Load]
parseLoad' (('[':xs):rest) =
    map (uncurry Loading) (parseShowModules [drop 11 $ dropWhile (/= ']') xs]) ++
    parseLoad rest
parseLoad' (x:xs)
    | not $ " " `isPrefixOf` x
    , (file,':':rest) <- break (== ':') x
    , takeExtension file `elem` [".hs",".lhs"]
    , (pos,rest2) <- span (\c -> c == ':' || isDigit c) rest
    , [p1,p2] <- map read $ words $ map (\c -> if c == ':' then ' ' else c) pos
    , (msg,las) <- span (isPrefixOf " ") xs
    , rest3 <- trimStart rest2
    , sev <- if "warning:" `isPrefixOf` lower rest3 then Warning else Error
    = Message sev file (p1,p2) (x:msg) : parseLoad las
parseLoad' (x:xs)
    | Just file <- stripPrefix "<no location info>: can't find file: " x
    = Message Error file (0,0) [file ++ ": Can't find file"] : parseLoad xs
parseLoad' (_:xs) = parseLoad xs
parseLoad' [] = []
