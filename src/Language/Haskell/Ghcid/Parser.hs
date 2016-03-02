{-# LANGUAGE PatternGuards #-}

-- | Parses the output from GHCi
module Language.Haskell.Ghcid.Parser(
    parseShowModules, parseLoad
    ) where

import System.FilePath
import Data.Char
import Data.List.Extra

import Language.Haskell.Ghcid.Types


-- | Parse messages from show modules command. Given the parsed lines
--   return a list of (module name, file).
parseShowModules :: [String] -> [(String, FilePath)]
parseShowModules xs =
    [ (takeWhile (not . isSpace) $ trimStart a, takeWhile (/= ',') b)
    | x <- xs, (a,'(':' ':b) <- [break (== '(') x]]


-- | Parse messages given on reload.
parseLoad :: [String] -> [Load]
-- nub, because cabal repl sometimes does two reloads at the start
parseLoad  = nubOrd . f
    where
        f :: [String] -> [Load]
        f (('[':xs):rest) =
            map (uncurry Loading) (parseShowModules [drop 11 $ dropWhile (/= ']') xs]) ++
            f rest
        f (x:xs)
            | not $ " " `isPrefixOf` x
            , (file,':':rest) <- break (== ':') x
            , takeExtension file `elem` [".hs",".lhs"]
            , (pos,rest2) <- span (\c -> c == ':' || isDigit c) rest
            , [p1,p2] <- map read $ words $ map (\c -> if c == ':' then ' ' else c) pos
            , (msg,las) <- span (isPrefixOf " ") xs
            , rest3 <- trimStart rest2
            , sev <- if "warning:" `isPrefixOf` lower rest3 then Warning else Error
            = Message sev file (p1,p2) (x:msg) : f las
        f (x:xs)
            | Just file <- stripPrefix "<no location info>: can't find file: " x
            = Message Error file (0,0) [file ++ ": Can't find file"] : f xs
        f (x:xs)
            | x == "Module imports form a cycle:"
            , (xs,rest) <- span (isPrefixOf " ") xs
            , let ms = [takeWhile (/= ')') x | x <- xs, '(':x <- [dropWhile (/= '(') x]]
            = Message Error "" (0,0) (x:xs) :
              -- need to label the modules in the import cycle so I can find them
              [Message Error m (0,0) [] | m <- nubOrd ms] ++ f rest
        f (_:xs) = f xs
        f [] = []
