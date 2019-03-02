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
import Data.Text                 ( Text         )
import Data.Char                 ( isDigit
                                 , isAlphaNum
                                 , isSpace      )
import Control.Applicative       ( (<|>)
                                 , Alternative
                                 , empty
                                 , liftA2
                                 , many
                                 , some         )

---------------------------------------------------------------------
-- Types

data Regex = Rule (Char -> Bool)
           | KleeneStar Regex
           | Epsilon
           | RegError

newtype Matcher a = Matcher { runMatcher :: Text -> [(Text, a)] }

instance Functor Matcher where
    fmap f (Matcher m) = Matcher $ \ t -> [ (t1, f x) | (t1, x) <- m t ]

instance Applicative Matcher where
    pure x                    = Matcher $ \ t -> [ (t, x) ]
    Matcher ml <*> Matcher mr = Matcher $ \ t ->
        [ (t2, f x) | (t1, f) <- ml t, (t2, x) <- mr t1 ]

instance Alternative Matcher where
    empty                     = Matcher $ \ t -> []
    Matcher ml <|> Matcher mr = Matcher $ \ t -> ml t <|> mr t

---------------------------------------------------------------------
-- Exported matching functions

(=~) :: String -> Text -> Bool
r =~ t = maybe False ( const True ) $ runRegex r t

hasMatch :: String -> Text -> Bool
hasMatch r = any (r =~) . Tx.tails

runRegex :: String -> Text -> Maybe (Text, String)
runRegex r t
    | null x = Nothing
    | otherwise = Just . head $ x
    where x = runMatcher (makeMatcher r) t

---------------------------------------------------------------------
-- Parsers to convert a string to a matcher

makeMatcher :: String -> Matcher String
makeMatcher = fmap concat <$> traverse regexToMatcher . makeRegex

regexToMatcher :: Regex -> Matcher String
regexToMatcher (Rule p)       = (:[]) <$> satisfy p
regexToMatcher (KleeneStar r) = fmap concat . many . regexToMatcher $ r
regexToMatcher Epsilon        = pure []
regexToMatcher RegError       = empty

makeRegex :: String -> [Regex]
makeRegex [] = [RegError]
makeRegex ('*':xs) = makeRegex xs
makeRegex ('+':xs) = makeRegex xs
makeRegex s        = reverse . go $ (s, Epsilon, [])
    where go ([],        r, rs) = r:rs
          go ('\\':x:xs, r, rs) = go (xs, parseEsc x, r:rs)
          go ('\\':[],   r, rs) = [RegError]
          go ('+':'+':xs,r, rs) = go ('+':xs, r, rs)
          go ('*':'+':xs,r, rs) = go ('*':xs, r, rs)
          go ('+':'*':xs,r, rs) = go ('*':xs, r, rs)
          go ('*':'*':xs,r, rs) = go ('*':xs, r, rs)
          go ('*':xs,    r, rs) = go (xs, Epsilon, KleeneStar r : rs)
          go ('+':xs,    r, rs) = go (xs, Epsilon, r : KleeneStar r : rs)
          go ('.':xs,    r, rs) = go (xs, Rule (/= '\n'), r : rs)
          go (x:xs,      r, rs) = go (xs, Rule (== x), r:rs)

parseEsc :: Char -> Regex
parseEsc 'd' = Rule isDigit
parseEsc 'D' = Rule $ not . isDigit
parseEsc 'w' = Rule isAlphaNum
parseEsc 'W' = Rule $ not . isAlphaNum
parseEsc 's' = Rule isSpace
parseEsc 'S' = Rule $ not . isSpace
parseEsc  x  = Rule (== x)

---------------------------------------------------------------------
-- Matchers

satisfy :: (Char -> Bool) -> Matcher Char
satisfy f = Matcher $ \ t -> maybe [] go . Tx.uncons $ t
    where go (x,xs) | f x       = [(xs, x)]
                    | otherwise = []
