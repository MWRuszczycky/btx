{-# LANGUAGE OverloadedStrings #-}

module BibTeX.Parser
    ( parseBib
    , parseRef
    ) where

import qualified Data.Attoparsec.Text as At
import qualified Data.Text            as Tx
import qualified Data.Map.Strict      as M
import qualified Types                as T
import Data.Bifunctor                       ( bimap       )
import Control.Applicative                  ( (<|>)       )
import Data.Text                            ( Text, pack  )
import Data.Char                            ( isAlphaNum  )

-- =============================================================== --
-- Types

-- |Convenience data type for entry-key/entry pairs.
type KeyEntry = (Text, T.Entry)

-- =============================================================== --
-- Main parsers

---------------------------------------------------------------------
-- Full bibliography parser

-- exported

parseBib :: FilePath -> Text -> Either String T.Bibliography
-- ^Exposed parser takes the file path to the .bib file and the input
-- text to parse.
parseBib fp = bimap (errorMessage fp) go . At.parseOnly bibParser
    where go x = T.Bibliography { T.path = fp
                                , T.refs = x  }

errorMessage :: FilePath -> String -> String
errorMessage fp err = unlines hs
    where hs = [ "Unable to parse " ++ fp ++ " as a BibTeX bibliography"
                 ++ " (.bib) file."
               , "The btx .bib parser is still fairly rudimentary and does not"
               , "yet support all the BibTeX features. In particular:"
               , "    1. Be sure that you are using the braced format."
               , "    2. Make sure that all comments are preceeded by a"
               , "       %-symbol just like in LaTeX."
               , "    3. The parser does not yet support @PREAMBLE or"
               , "       @COMMENT entries."
               , "    4. The parser does not yet support use of the"
               , "       #-concatenation operator.\n"
               , "Additional information from the parser: " ++ err
               ]

bibParser :: At.Parser T.References
-- ^Main file parser for reading each BibTeX reference.
bibParser = do
    ignored
    rs <- At.many' reference
    At.endOfInput
    return . M.fromList $ rs

---------------------------------------------------------------------
-- Single reference parser
-- Used for parsing doi dowloads and externally edited references.

-- exported

parseRef :: FilePath -> Text -> Either String T.Ref
-- ^Parse a single BibTeX entry from a Text string.
parseRef fp x = do
    (k, v) <- At.parseOnly oneRef x
    return $ T.Ref fp k v

-- unexported

oneRef :: At.Parser KeyEntry
-- ^Parses a single isolated entry from a Text string.
oneRef = do
    ignored
    r <- reference
    At.endOfInput
    return r

-- =============================================================== --
-- Unexported helper parsers

---------------------------------------------------------------------
-- Parsing references

reference :: At.Parser KeyEntry
-- ^Parses an individual BibTeX entry. Metadata associated with the
-- reference immediately follows the reference entry. All other
-- comments are ignored.
reference = do
    (rt, rk) <- refHeader
    At.skipSpace
    fields <- At.many' kvPair
    -- Entries can end with either a brace or comma-spaces-brace.
    ( At.char ',' >> At.skipSpace >> At.char '}' ) <|> At.char '}'
    skipLineSpaces
    next <- At.peekChar'
    case next of
         -- The next reference starts on the same line as the closing brace.
         '@' -> return ( rk, T.Entry rt fields [] )
         -- There is meta data after the closing brace.
         '%' -> do cs <- At.many' metaData
                   ignored
                   return ( rk, T.Entry rt fields cs )
         -- Otherwise there can only be spaces after the closing brace.
         _   -> do cs <- At.endOfLine >> At.many' metaData
                   ignored
                   return ( rk, T.Entry rt fields cs)

---------------------------------------------------------------------
-- Parsing reference headers

refHeader :: At.Parser (Text, Text)
-- ^Parses the entry header to obtain the BibTeX entry type such as
-- an 'article' or 'book' and then the reference key.
refHeader = do
    rt <- refType
    At.skipSpace
    rk <- refKey
    return (rt, rk)

refType :: At.Parser Text
-- ^Parses the reference entry type.
refType = do
    At.char '@'
    At.skipSpace
    rt <- At.takeWhile1 ( At.notInClass "{ \t\n\r\f\v" )
    At.skipSpace
    At.char '{'
    return rt

refKey :: At.Parser Text
-- ^Parses the reference entry key.
refKey = At.takeWhile1 ( At.notInClass ",{}@ \t\n\r\f\v" )

---------------------------------------------------------------------
-- Parsing key-value pairs

kvPair :: At.Parser T.Field
-- ^Parse individiual key-value pairs in the BibTeX reference fields.
-- Values must be enclosed by braces and not double quotes.
kvPair = do
    At.char ','
    At.skipSpace
    k <- keyParse
    At.skipSpace
    At.char '='
    At.skipSpace
    v <- valParse
    At.skipSpace
    return (k, v)

keyParse :: At.Parser Text
-- ^Parser for BibTeX reference field keys, which must be alphanumeric
-- may only contain the alphanumeric '-' or '_' characters.
keyParse = pack <$> At.many' ( At.satisfy good )
    where good x = isAlphaNum x || x == '-' || x == '_'

valParse :: At.Parser Text
-- ^Parser for BibTeX reference field values. These must be enclosed
-- in braces and not double quotes.
valParse = do
    At.char '{'
    vs <- value
    return vs

value :: At.Parser Text
-- ^Recursive parser for BibTeX reference field values.
value = do
    start <- At.takeWhile ( \ x -> x /= '{' && x /= '}' && x /= '\\' )
    next  <- At.peekChar'
    case next of
         '\\' -> do escaped <- At.take 2
                    rest    <- value
                    return $ start <> escaped <> rest
         '{'  -> do inBraces <- valParse
                    rest     <- value
                    return $ start <> ( "{" <> inBraces <> "}" ) <> rest
         _    -> At.char '}' >> return start

---------------------------------------------------------------------
-- Parsing comments and ignored material

ignored :: At.Parser ()
-- ^Space and comments between entries that should be ignored.
ignored = At.many' ignoredLine >> At.skipSpace

skipRestOfLine :: At.Parser ()
skipRestOfLine = At.skipWhile ( not . At.isEndOfLine ) >> At.endOfLine

ignoredLine :: At.Parser ()
-- ^Empty lines and comments that are ignored.
ignoredLine = do
    At.skipSpace
    At.char '%' >> skipRestOfLine <|> At.endOfLine

metaData :: At.Parser Text
metaData = do
    At.char '%'
    skipLineSpaces
    c <- pack <$> ( At.manyTill At.anyChar $ At.endOfLine )
    skipLineSpaces
    return c

---------------------------------------------------------------------
-- Miscellaneous parsing

skipLineSpaces :: At.Parser ()
-- ^Skip spaces and tabs but not newlines.
skipLineSpaces = At.skipWhile ( \ x -> x == ' ' || x == '\t' )
