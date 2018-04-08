{-# LANGUAGE PatternGuards #-}

-- | Module for dealing with escape codes
module Language.Haskell.Ghcid.Escape(
    Esc(..), unescape,
    stripInfixE, stripPrefixE, isPrefixOfE, spanE, trimStartE, unwordsE
    ) where

import Data.Char
import Data.Either.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Control.Applicative
import Prelude


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
