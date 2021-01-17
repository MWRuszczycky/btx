{-# LANGUAGE OverloadedStrings #-}

module Model.Matcher
    ( (=~)
    , hasMatch
    , runRegex
    ) where

-- =============================================================== --
-- Very rudimentary implementation of a regular expression matcher --
-- using an applicative parser over Text. This is used for         --
-- implementing the <find> command.                                --
-- =============================================================== --

import qualified Data.Text                as Tx
import           Data.Text                      ( Text         )
import           Data.Char                      ( isDigit
                                                , isAlphaNum
                                                , isSpace      )
import           Control.Monad.State.Lazy       ( (<=<)
                                                , StateT
                                                , StateT (..)
                                                , runStateT    )
import           Control.Applicative            ( many, empty  )

---------------------------------------------------------------------
-- Types

data Regex = Rule (Char -> Bool) | KleeneStar Regex

type Matcher a = StateT Text [] a

---------------------------------------------------------------------
-- Exported matching functions

(=~) :: String -> Text -> Bool
r =~ t = maybe False ( const True ) $ runRegex r t

hasMatch :: String -> Text -> Bool
hasMatch r = any (r =~) . Tx.tails

runRegex :: String -> Text -> Maybe (String, Text)
runRegex r t
    | null x    = Nothing
    | otherwise = Just . head $ x
    where x = runStateT ( buildMatcher r ) t

---------------------------------------------------------------------
-- Parsers to convert a string to a matcher

buildMatcher :: String -> Matcher String
buildMatcher = maybe empty go . parseRegex
    where go = fmap concat <$> traverse compileRegex

compileRegex :: Regex -> Matcher String
compileRegex (Rule p)       = (:[]) <$> satisfy p
compileRegex (KleeneStar r) = fmap concat . many . compileRegex $ r

parseRegex :: String -> Maybe [Regex]
parseRegex = go <=< nextRegex
    where go (r,[]) = Just [r]
          go (r,xs) = fmap (r:) . parseRegex $ xs

nextRegex :: String -> Maybe (Regex, String)
nextRegex []          = Nothing
nextRegex ('*':xs)    = nextRegex xs
nextRegex ('\\':x:xs) = readStars . ( \ y -> (y, xs) ) <$> parseEsc x
nextRegex ('.':xs)    = Just . readStars $ ( Rule (/= '\n'), xs )
nextRegex (x:xs)      = Just . readStars $ ( Rule (== x),    xs )

readStars :: (Regex, String) -> (Regex, String)
readStars (r, s)
    | null xs   = (r, s)
    | otherwise = (KleeneStar r, ys)
    where (xs,ys) = span (== '*') s

parseEsc :: Char -> Maybe Regex
parseEsc 'd'  = Just . Rule $ isDigit
parseEsc 'D'  = Just . Rule $ not . isDigit
parseEsc 'w'  = Just . Rule $ isAlphaNum
parseEsc 'W'  = Just . Rule $ not . isAlphaNum
parseEsc 's'  = Just . Rule $ isSpace
parseEsc 'S'  = Just . Rule $ not . isSpace
parseEsc '\\' = Just . Rule $ (== '\\')
parseEsc '*'  = Just . Rule $ (== '*')
parseEsc '.'  = Just . Rule $ (== '.')
parseEsc  _   = Nothing

---------------------------------------------------------------------
-- Matchers

satisfy :: (Char -> Bool) -> Matcher Char
satisfy f = StateT $ \ t -> maybe [] go . Tx.uncons $ t
    where go (x,xs) | f x       = [(x,xs)]
                    | otherwise = []
