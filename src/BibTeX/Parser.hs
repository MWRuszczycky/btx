{-# LANGUAGE OverloadedStrings #-}

module BibTeX.Parser
    ( parseBib
     --, parseRef
    ) where

import qualified Data.Attoparsec.Text as At
import Data.Attoparsec.Text ( (<?>) )
import Control.Applicative ( (<|>) )
import Data.Text (Text, pack)
import Data.Char ( isAlphaNum )
import qualified Types as T
import Data.Map.Strict as M

parseBib :: FilePath -> Text -> Either String T.Bibliography
-- ^Exposed parser takes the file path to the .bib file and the input
-- text to parse.
parseBib fp x = do
    refs <- At.parseOnly bibParser x
    return T.Bibliography { T.path = fp
                          , T.refs = refs }

---------------------------------------------------------------------
-- Hidden helper functions
---------------------------------------------------------------------

type KeyEntry = (Text, T.Entry)

---------------------------------------------------------------------
-- Main parser

bibParser :: At.Parser T.References
-- ^Main file parser for reading each BibTeX reference.
bibParser = do
    At.skipWhile ( /= '@' )
-- At.skipMany $ emptyLine <|> ignoredComment
    rs <- At.many' reference
    At.endOfInput
    return . M.fromList $ rs

oneRef :: At.Parser KeyEntry
-- ^Parses a single isolated reference from a string. This is used for
-- parsing doi dowloads.
oneRef = spaces >> reference

---------------------------------------------------------------------
-- Parsing references

reference :: At.Parser KeyEntry
-- ^Parses an individual BibTeX reference and consumes all following
-- space until either another reference is encountered or the file
-- ends.
reference = do
    (rt, rk) <- refHeader
    fields <- kvPairs
    sep <|> At.skipSpace
    At.char '}'
    cs <- commentLines
    At.skipWhile ( /= '@' )
    return $ ( rk, T.Entry rt fields cs )

---------------------------------------------------------------------
-- Parsing reference headers

refHeader :: At.Parser (Text, Text)
-- ^Parses the reference header to obtain the BibTeX reference type
-- such as an 'article' or 'book' and then the reference key. There
-- is no checking for repeated keys.
refHeader = do
    rt <- refType
    At.skipSpace
    rk <- refKey
    At.skipSpace
    sep
    return (rt, rk)

refType :: At.Parser Text
-- ^Parses the reference type which must have no space between the
-- '@' symbol and the opening brace. For example, '@article{' will
-- parse; however, '@ article{' and '@article {' will not.
refType = do
    At.char '@' <?> "a new BibTeX reference"
    rt <- At.takeWhile ( At.notInClass "{ " )
    At.char '{'
    return rt

refKey :: At.Parser Text
-- ^Parses an reference key, which must be alpha-numeric and may only
-- include the special characters '-', '_', '>' and '<'.
refKey = pack <$> At.many' ( At.satisfy good )
    where good x = isAlphaNum x || At.inClass "<>-_" x

---------------------------------------------------------------------
-- Parsing key-value pairs

kvPairs :: At.Parser [ T.Field ]
-- ^At.Parser or all key-value pairs in the BibTeX reference fields.
kvPairs = do
    firstPair <- kvPair
    lastPairs <- At.many' ( sep >> kvPair )
    return $ firstPair : lastPairs

kvPair :: At.Parser T.Field
-- ^At.Parser for individiual key-value pairs in the BibTeX reference
-- fields. Keys must be alpha-numeric, and values must be enclosed by
-- braces and not double quotes.
kvPair = do
    key <- keyParse
    equals
    val <- valParse
    spaces
    return (key, val)

keyParse :: At.Parser Text
-- ^At.Parser for BibTeX reference field keys.
keyParse = pack <$> At.many' ( At.satisfy isAlphaNum )

valParse :: At.Parser Text
-- ^At.Parser for BibTeX reference field values. These must be enclosed in
-- braces and not double quotes. Currently, characters of the form
-- '\{' and '\}' are supported only if they are paired. See value
-- parser monad for details.
valParse = do
    At.char '{'
    vs <- value
    return vs

value :: At.Parser Text
-- ^Recursive parser for BibTeX reference field values. Note that
-- even escaped braces (i.e., '\{' and '\}') need to be paired in
-- this implementation.
value = do
    ls <- pack <$> At.many' ( At.satisfy ( At.notInClass "{}" ) )
    b  <- At.satisfy ( At.inClass "{}" )
    case b of
         '}' -> return ls
         '{' -> do cs <- value
                   rs <- value
                   return $ ls <> ( enbrace cs ) <> rs

---------------------------------------------------------------------
-- Parsing comments

ignoredComment :: At.Parser ()
-- ^For parsing comment lines that are to be ignored.
ignoredComment = do
    whiteSpace
    At.char '%'
    At.manyTill At.anyChar eol
    return ()

commentLines :: At.Parser [ Text ]
-- ^For parsing all comments associated with a reference that will be
-- recorded.
commentLines = do
    c <- earlyComment
    cs <- At.many' commentLine
    case c of
         "" -> return cs
         _  -> return $ c:cs

commentLine :: At.Parser Text
-- ^Parses a recorded comment line.
commentLine = do
    At.char '%'
    whiteSpace
    s <- pack <$> ( At.manyTill At.anyChar $ eol )
    whiteSpace
    return s

earlyComment :: At.Parser Text
-- ^Comment lines that follow a reference closing brace without a new
-- line and are to be recorded.
earlyComment = do
    whiteSpace
    x <- At.peekChar
    case x of
         Just '%' -> commentLine
         Just x   -> eol >> return ""
         Nothing  -> return ""

---------------------------------------------------------------------
-- Miscellaneous parsing

eol :: At.Parser Char
-- ^At.Parser for different end of line strings. If the parser succeeds,
-- then the end of line string is consumed and the '\n' character
-- alone is returned. If the parser fails, then nothing is consumed.
eol = At.choice [ At.string "\r\n" >> return '\n'
                , At.string "\n\r" >> return '\n'
                , At.char '\n'
                ]

sep :: At.Parser ()
-- ^At.Parser for comma separators that consumes all following spaces.
sep = At.char ',' >> spaces

equals :: At.Parser ()
-- ^At.Parser for equals symbol that consumes all preceeding and
-- following spaces.
equals = spaces >> At.char '=' >> spaces

whiteSpace :: At.Parser ()
-- ^Spaces but not newlines.
whiteSpace = At.skipMany $ At.satisfy ( At.inClass " \t" )

emptyLine :: At.Parser ()
-- ^For parsing empty lines that are to be ignored.
emptyLine = whiteSpace >> eol >> return ()

spaces :: At.Parser ()
spaces = At.skipMany At.space

---------------------------------------------------------------------
-- Nonparsing helper functions

enbrace :: Text -> Text
-- ^Enclose a string in braces.
enbrace x = "{" <> x <> "}"
