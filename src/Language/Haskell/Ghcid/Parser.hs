{-# LANGUAGE PatternGuards, ViewPatterns #-}

-- | Parses the output from GHCi
module Language.Haskell.Ghcid.Parser(
    parseShowModules, parseLoad
    ) where

import System.FilePath
import Data.Char
import Data.Either.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Control.Applicative
import Prelude

import Language.Haskell.Ghcid.Types

-- A string with escape characters in it
newtype Esc = Esc {fromEsc :: String}

app (Esc x) (Esc y) = Esc $ x ++ y

unesc :: Esc -> Maybe (Either Esc Char, Esc)
unesc (Esc ('\ESC':xs)) | (pre,'m':post) <- break (== 'm') xs = Just (Left $ Esc $ '\ESC':pre++"m", Esc post)
unesc (Esc (x:xs)) = Just (Right x, Esc xs)
unesc (Esc []) = Nothing

-- | Remove all escape characters in a string
unescape :: Esc -> String
unescape = rights . unfoldr unesc


-- | Parse messages from show modules command. Given the parsed lines
--   return a list of (module name, file).
parseShowModules :: [String] -> [(String, FilePath)]
parseShowModules (map Esc -> xs) =
    -- we only return raw values, don't want any escape codes in there
    [ (takeWhile (not . isSpace) $ trimStart a, takeWhile (/= ',') b)
    | x <- map unescape xs, (a,'(':' ':b) <- [break (== '(') x]]

stripPrefixE :: String -> Esc -> Maybe Esc
stripPrefixE [] e = Just e
stripPrefixE (x:xs) e = case unesc e of
    Just (Left code, rest) -> fmap (app code) $ stripPrefixE (x:xs) rest
    Just (Right y, rest) | y == x -> stripPrefixE xs rest
    _ -> Nothing

stripInfixE :: String -> Esc -> Maybe (Esc, Esc)
stripInfixE needle haystack | Just rest <- stripPrefixE needle haystack = Just (Esc [], rest)
stripInfixE needle e = case unesc e of
    Nothing -> Nothing
    Just (x,xs) -> first (app $ fromEither $ fmap (Esc . return) x) <$> stripInfixE needle xs


spanE :: (Char -> Bool) -> Esc -> (Esc, Esc)
spanE f e = case unesc e of
    Nothing -> (Esc "", Esc "")
    Just (Left e, rest) -> first (app e) $ spanE f rest
    Just (Right c, rest) | f c -> first (app $ Esc [c]) $ spanE f rest
                         | otherwise -> (Esc "", e)

isPrefixOfE :: String -> Esc -> Bool
isPrefixOfE x y = isJust $ stripPrefixE x y

trimStartE :: Esc -> Esc
trimStartE e = case unesc e of
    Nothing -> Esc ""
    Just (Left code, rest) -> app code $ trimStartE rest
    Just (Right c, rest) | isSpace c -> trimStartE rest
                         | otherwise -> e


unwordsE :: [Esc] -> Esc
unwordsE = Esc . unwords . map fromEsc

-- | Parse messages given on reload.
parseLoad :: [String] -> [Load]
-- nub, because cabal repl sometimes does two reloads at the start
parseLoad (map Esc -> xs) = nubOrd $ f xs
    where
        f :: [Esc] -> [Load]

        -- [1 of 2] Compiling GHCi             ( GHCi.hs, interpreted )
        f (xs:rest)
            | Just xs <- stripPrefixE "[" xs
            = map (uncurry Loading) (parseShowModules [drop 11 $ dropWhile (/= ']') $ unescape xs]) ++
              f rest

        -- GHCi.hs:81:1: Warning: Defined but not used: `foo'
        f (x:xs)
            | not $ " " `isPrefixOfE` x
            , Just (file,rest) <- breakFileColon x
            , takeExtension file `elem` [".hs",".lhs",".hs-boot",".lhs-boot"]
             -- take position, including span if present
            , (pos,rest) <- spanE (\c -> c == ':' || c == '-' || isSpan c || isDigit c) rest
            -- separate line and column, ignoring span (we want the start point only)
            , [p1,p2] <- map read $ wordsBy (\c -> c == ':' || isSpan c) $ takeWhile (/= '-') $ unescape pos
            , (msg,las) <- span isMessageBody xs
            , rest <- trimStartE $ unwordsE $ rest : xs
            , sev <- if "warning:" `isPrefixOf` lower (unescape rest) then Warning else Error
            = Message sev file (p1,p2) (map unescape $ x:msg) : f las

        -- <no location info>: can't find file: FILENAME
        f (x:xs)
            | Just file <- stripPrefixE "<no location info>: can't find file: " x
            = Message Error (unescape file) (0,0) [unescape file ++ ": Can't find file"] : f xs

        -- Module imports form a cycle:
        --   module `Module' (Module.hs) imports itself
        f (x:xs)
            | unescape x == "Module imports form a cycle:"
            , (xs,rest) <- span (isPrefixOfE " ") xs
            , let ms = [takeWhile (/= ')') x | x <- xs, '(':x <- [dropWhile (/= '(') $ unescape x]]
            = Message Error "" (0,0) (map unescape $ x:xs) :
              -- need to label the modules in the import cycle so I can find them
              [Message Error m (0,0) [] | m <- nubOrd ms] ++ f rest
        f (_:xs) = f xs
        f [] = []
        isSpan c = c== ',' || c == '(' || c == ')'

-- After the file location, message bodies are indented (perhaps prefixed by a line number)
isMessageBody :: Esc -> Bool
isMessageBody xs = isPrefixOfE " " xs || case stripInfixE "|" xs of
  Just (prefix, _) | all (\x -> isSpace x || isDigit x) $ unescape prefix -> True
  _ -> False

-- A filename, followed by a colon - be careful to handle Windows drive letters, see #61
breakFileColon :: Esc -> Maybe (FilePath, Esc)
breakFileColon xs = case stripInfixE ":" xs of
    Nothing -> Nothing
    Just (a,b)
        | [drive] <- unescape a, isLetter drive -> first ((++) [drive,':'] . unescape) <$> stripInfixE ":" b
        | otherwise -> Just (unescape a, b)
