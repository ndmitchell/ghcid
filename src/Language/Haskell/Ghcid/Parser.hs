{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | Parses the output from GHCi
module Language.Haskell.Ghcid.Parser(
    parseShowModules, parseShowPaths, parseLoad
    ) where

import System.FilePath
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Control.Applicative
import Prelude

import Language.Haskell.Ghcid.Types
import Language.Haskell.Ghcid.Escape


-- | Parse messages from show modules command. Given the parsed lines
--   return a list of (module name, file).
parseShowModules :: [String] -> [(String, FilePath)]
parseShowModules (map unescape -> xs) =
    -- we only return raw values, don't want any escape codes in there
    [ (takeWhile (not . isSpace) $ trimStart a, takeWhile (/= ',') b)
    | x <- xs, (a,'(':' ':b) <- [break (== '(') x]]

-- | Parse messages from show paths command. Given the parsed lines
--   return (current working directory, module import search paths)
parseShowPaths :: [String] -> (FilePath, [FilePath])
parseShowPaths (map unescape -> xs)
    | (_:x:_:is) <- xs = (trimStart x, map trimStart is)
    | otherwise = (".",[])

-- | Parse messages given on reload.
parseLoad :: [String] -> [Load]
-- nub, because cabal repl sometimes does two reloads at the start
parseLoad (map Esc -> xs) = nubOrd $ f xs
    where
        f :: [Esc] -> [Load]

        -- [1 of 2] Compiling GHCi             ( GHCi.hs, interpreted )
        f (xs:rest)
            | Just xs <- stripPrefixE "[" xs
            = map (uncurry Loading) (parseShowModules [drop 11 $ dropWhile (/= ']') $ unescapeE xs]) ++
              f rest

        -- GHCi.hs:81:1: Warning: Defined but not used: `foo'
        f (x:xs)
            | not $ " " `isPrefixOfE` x
            , Just (file,rest) <- breakFileColon x
            , takeExtension file `elem` [".hs",".lhs",".hs-boot",".lhs-boot"]
             -- take position, including span if present
            , (pos,rest) <- spanE (\c -> c == ':' || c == '-' || isSpan c || isDigit c) rest
            -- separate line and column, ignoring span (we want the start point only)
            , [p1,p2] <- map read $ wordsBy (\c -> c == ':' || isSpan c) $ takeWhile (/= '-') $ unescapeE pos
            , (msg,las) <- span isMessageBody xs
            , rest <- trimStartE $ unwordsE $ rest : xs
            , sev <- if "warning:" `isPrefixOf` lower (unescapeE rest) then Warning else Error
            = Message sev file (p1,p2) (map fromEsc $ x:msg) : f las

        -- <no location info>: can't find file: FILENAME
        f (x:xs)
            | Just file <- stripPrefixE "<no location info>: can't find file: " x
            = Message Error (unescapeE file) (0,0) [unescapeE file ++ ": Can't find file"] : f xs

        -- Module imports form a cycle:
        --   module `Module' (Module.hs) imports itself
        f (x:xs)
            | unescapeE x == "Module imports form a cycle:"
            , (xs,rest) <- span (isPrefixOfE " ") xs
            , let ms = [takeWhile (/= ')') x | x <- xs, '(':x <- [dropWhile (/= '(') $ unescapeE x]]
            = Message Error "" (0,0) (map fromEsc $ x:xs) :
              -- need to label the modules in the import cycle so I can find them
              [Message Error m (0,0) [] | m <- nubOrd ms] ++ f rest

        -- Loaded GHCi configuration from C:\Neil\ghcid\.ghci
        f (x:xs)
            | Just x <- stripPrefixE "Loaded GHCi configuration from " x
            = LoadConfig (unescapeE x) : f xs

        f (_:xs) = f xs
        f [] = []
        isSpan c = c== ',' || c == '(' || c == ')'

-- After the file location, message bodies are indented (perhaps prefixed by a line number)
isMessageBody :: Esc -> Bool
isMessageBody xs = isPrefixOfE " " xs || case stripInfixE "|" xs of
  Just (prefix, _) | all (\x -> isSpace x || isDigit x) $ unescapeE prefix -> True
  _ -> False

-- A filename, followed by a colon - be careful to handle Windows drive letters, see #61
breakFileColon :: Esc -> Maybe (FilePath, Esc)
breakFileColon xs = case stripInfixE ":" xs of
    Nothing -> Nothing
    Just (a,b)
        | [drive] <- unescapeE a, isLetter drive -> first ((++) [drive,':'] . unescapeE) <$> stripInfixE ":" b
        | otherwise -> Just (unescapeE a, b)
