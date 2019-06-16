{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Matcher
    ( (=~)
    , hasMatch
    , runRegex
    ) where

-- =============================================================== --
-- Very rudimentary implementation of a regular expression matcher --
-- using an applicative parser over Text. This is used for         --
-- implementing the <find> command.                                --
-- =============================================================== --

import qualified Data.Text as Tx
import Data.Text                   ( Text         )
import Data.Char                   ( isDigit
                                   , isAlphaNum
                                   , isSpace      )
import Control.Monad.State.Lazy    ( StateT
                                   , StateT (..)
                                   , runStateT    )
import Control.Applicative         ( many, empty  )

---------------------------------------------------------------------
-- Types

data Regex
    = Rule (Char -> Bool)
    | KleeneStar Regex
    | Epsilon
    | RegError

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
buildMatcher = fmap concat <$> traverse compileRegex . parseRegex

compileRegex :: Regex -> Matcher String
compileRegex (Rule p)       = (:[]) <$> satisfy p
compileRegex (KleeneStar r) = fmap concat . many . compileRegex $ r
compileRegex Epsilon        = pure []
compileRegex RegError       = empty

parseRegex :: String -> [Regex]
parseRegex [] = [RegError]
parseRegex ('*':xs) = parseRegex xs
parseRegex s        = reverse . go $ (s, Epsilon, [])
    where go ([],        r, rs) = r:rs
          go ('\\':x:xs, r, rs) = go (xs, parseEsc x, r:rs)
          go ('\\':[],   _, _ ) = [RegError]
          go ('*':'*':xs,r, rs) = go ('*':xs, r, rs)
          go ('*':xs,    r, rs) = go (xs, Epsilon, KleeneStar r : rs)
          go ('.':xs,    r, rs) = go (xs, Rule (/= '\n'), r : rs)
          go (x:xs,      r, rs) = go (xs, Rule (== x), r:rs)

parseEsc :: Char -> Regex
parseEsc 'd'  = Rule isDigit
parseEsc 'D'  = Rule $ not . isDigit
parseEsc 'w'  = Rule isAlphaNum
parseEsc 'W'  = Rule $ not . isAlphaNum
parseEsc 's'  = Rule isSpace
parseEsc 'S'  = Rule $ not . isSpace
parseEsc '\\' = Rule $ (== '\\')
parseEsc '*'  = Rule $ (== '*')
parseEsc '.'  = Rule $ (== '.')
parseEsc  _   = RegError

---------------------------------------------------------------------
-- Matchers

satisfy :: (Char -> Bool) -> Matcher Char
satisfy f = StateT $ \ t -> maybe [] go . Tx.uncons $ t
    where go (x,xs) | f x       = [(x,xs)]
                    | otherwise = []
