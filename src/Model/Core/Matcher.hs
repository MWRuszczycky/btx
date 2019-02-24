{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Matcher
    ( (=~)
    , hasMatch
    , runRegex
    ) where

-- =============================================================== --
-- Very rudimentary implementation of regular expressions using an --
-- applicative parser over Text. This is used for implementing the --
-- <find> command.                                                 --

-- The behavior of the matcher is not strictly correct. For        --
-- example, "c.*s" will not match "cats" even though it should.    --
-- However, "c.*" will match "cats", as it should. To get this to  --
-- work correctly will require changing the matcher so that it     --
-- saves intermediate prefix matches in a list rather than using a --
-- Maybe in order to allow backtracking. In the end, this module   --
-- will probably be scrapped and the functionallity replace by     --
-- something written by somebody who knows what they are doing :)  --
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
-- Types. These will eventually be moved to the Types module unless
-- this whole thing gets scrapped.

-- |Model for a very simple regular expression
data Regex = Symbol Char
           | Rule (Char -> Bool)
           | KleenePlus Regex
           | KleeneStar Regex
           | Epsilon
           | RegError

instance Show Regex where
    show ( Symbol x     ) = [x]
    show ( Rule _       ) = "rule"
    show ( KleenePlus r ) = show r ++ "+"
    show ( KleeneStar r ) = show r ++ "*"
    show ( Epsilon      ) = "@"
    show ( RegError     ) = "Error"

-- |Regular expression matcher for Text that uses applicative parsing
newtype Matcher a = Matcher { runMatcher :: Text -> Maybe (Text, a) }

instance Functor Matcher where
    fmap f (Matcher m) = Matcher $ \ t -> do (t1, x) <- m t
                                             pure (t1, f x)

instance Applicative Matcher where
    pure x                    = Matcher $ \ t -> pure (t, x)
    Matcher ml <*> Matcher mr = Matcher $ \ t -> do (t1, f) <- ml t
                                                    (t2, x) <- mr t1
                                                    pure (t2, f x)

instance Alternative Matcher where
    empty                     = Matcher $ \ t -> Nothing
    Matcher ml <|> Matcher mr = Matcher $ \ t -> ml t <|> mr t

---------------------------------------------------------------------
-- Exported matching functions

(=~) :: String -> Text -> Bool
r =~ t = maybe False ( const True ) $ runRegex r t

hasMatch :: String -> Text -> Bool
hasMatch r = any (r =~) . Tx.tails

runRegex :: String -> Text -> Maybe (Text, String)
runRegex r = runMatcher (makeMatcher r)

---------------------------------------------------------------------
-- Parsers to convert a string to a matcher

makeMatcher :: String -> Matcher String
makeMatcher = fmap concat <$> traverse regexToMatcher . makeRegex

regexToMatcher :: Regex -> Matcher String
regexToMatcher (Symbol c)     = (:[]) <$> char c
regexToMatcher (Rule p)       = (:[]) <$> satisfy p
regexToMatcher (KleenePlus r) = fmap concat . some . regexToMatcher $ r
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
          go ('+':xs,    r, rs) = go (xs, Epsilon, KleenePlus r : rs)
          go ('.':xs,    r, rs) = go (xs, Rule $ (/= '\n'), r : rs)
          go (x:xs,      r, rs) = go (xs, Symbol x, r:rs)

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
satisfy f = Matcher $ \ t -> do (x, xs) <- Tx.uncons t
                                if f x then Just (xs, x) else Nothing

char :: Char -> Matcher Char
char c = satisfy (== c)
