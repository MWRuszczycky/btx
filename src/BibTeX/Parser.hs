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
import Data.Char                            ( isAlphaNum
                                            , isSpace     )

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
    where go (h, rs) = T.Bibliography { T.path   = fp
                                      , T.refs   = rs
                                      , T.header = h
                                      }

errorMessage :: FilePath -> String -> String
errorMessage fp err = unlines hs
    where hs = [ "Unable to parse " ++ fp ++ " as a BibTeX bibliography"
                 ++ " (.bib) file."
               , "The btx BibTeX parser requires that:"
               , "    1. All reference entries use the braced format."
               , "    2. @PREAMBLE and @STRING entries precede all other"
               , "       entries. In contrast, @COMMENT entries can follow"
               , "       reference entries and will be parsed as metadata"
               , "       associated with the reference entry they follow.\n"
               , "Additional information from the parser: " ++ err
               ]

bibParser :: At.Parser (Text, T.References)
-- ^Main file parser for reading each BibTeX reference.
bibParser = do
    hdr <- Tx.strip <$> header
    rs  <- At.many' reference
    At.endOfInput
    return ( hdr, M.fromList rs )

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
    At.skipWhile (/= '@')
    r <- reference
    At.endOfInput
    return r

-- =============================================================== --
-- Unexported helper parsers

---------------------------------------------------------------------
-- Parsing references

reference :: At.Parser KeyEntry
-- ^Parses an individual BibTeX entry. All comments that follow the
-- reference are treated as metadata associated with the reference.
reference = do
    (rt, rk) <- refHeader
    At.skipSpace
    fields <- At.many' kvPair
    -- Entries can end with either a brace or comma-spaces-brace.
    ( At.char ',' >> At.skipSpace >> At.char '}' ) <|> At.char '}'
    md <- metadata
    return ( rk, T.Entry rt fields md )

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
-- Parsing metadata and headers

metadata :: At.Parser [Text]
metadata = do
    xs <- formatMeta <$> At.takeWhile ( /= '@' )
    c  <- metaBibTeX "comment" <|> return Tx.empty
    if Tx.null c
       then return xs
       else do rest <- metadata
               return $ xs ++ [c] ++ rest

header :: At.Parser Text
header = do
    xs <- At.takeWhile ( /= '@' )
    u  <- anyMetaBibTeX
    if Tx.null u
       then return xs
       else do rest <- header
               return $ xs <> u <> rest

anyMetaBibTeX :: At.Parser Text
anyMetaBibTeX = metaBibTeX "comment"
                <|> metaBibTeX "string"
                <|> metaBibTeX "preamble"
                <|> return Tx.empty

metaBibTeX :: Text -> At.Parser Text
metaBibTeX t = do
    At.char '@'
    At.skipSpace
    At.asciiCI t
    return $ Tx.cons '@' t

formatMeta :: Text -> [Text]
formatMeta = filter (not . Tx.null) . map (Tx.dropWhile isSpace ) . Tx.lines
