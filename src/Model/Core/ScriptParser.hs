{-# LANGUAGE OverloadedStrings #-}

module Model.Core.ScriptParser
    ( parse
    ) where

-- =============================================================== --
-- Module for parsing btx scripts
-- =============================================================== --

import qualified Data.Attoparsec.Text   as At
import qualified Data.Text              as Tx
import Control.Applicative                    ( (<|>)       )
import Control.Monad                          ( guard       )
import Data.Char                              ( isSpace     )
import Data.Text                              ( Text
                                              , pack
                                              , unpack      )
import qualified Model.Core.Types as T
-- import Model.Core.Messages.Help               ( versionStr  )

---------------------------------------------------------------------
-- New (not working yet)

-- |Starting state: Need to change how the start state is prepared
data ParsedInput = Help [String]
                 | Usage String
                 | Normal [String]
                 deriving ( Show )

parse' :: String -> ParsedInput
parse' s = case At.parseOnly scriptParser (pack s) of
                Left e   -> Usage $ "Unable to parse script (try: btx help).\n"
                Right cs -> cs

scriptParser :: At.Parser ParsedInput
scriptParser = do
    At.skipSpace
    At.choice [ cryForHelp, versionRequest, btxScript ]

cryForHelp :: At.Parser ParsedInput
cryForHelp = do
    At.choice [ At.string "help"
              , At.string "--help"
              , At.string "-h" ]
    At.takeText >>= pure . Help . words . unpack

versionRequest :: At.Parser ParsedInput
versionRequest = do
    At.choice [ At.string "version"
              , At.string "--version"
              , At.string "-v" ]
    pure . Usage $ "version string here" -- versionStr

btxScript :: At.Parser ParsedInput
btxScript = At.many' aToken >>= pure . Normal

aToken :: At.Parser String
aToken = do
    skipSimpleSpace
    aWord <|> aQuotedString <|> aSymbol

aWord :: At.Parser String
aWord = At.many1' $ At.satisfy (At.notInClass "+, \n\r'\"")

aQuotedString :: At.Parser String
-- Needs to be fixed: internal '\"' will not be recognized.
aQuotedString = do
    open <- At.char '\"' <|> At.char '\''
    At.manyTill At.anyChar (At.char open)

aSymbol :: At.Parser String
aSymbol = withSymbol <|> andSymbol1 <|> andSymbol2 <|> andSymbol3
    where withSymbol = At.char '+'      *> pure "with"
          andSymbol1 = At.char ','      *> pure "and"
          andSymbol2 = At.char '\n'     *> pure "and"
          andSymbol3 = At.string "\n\r" *> pure "and"

skipSimpleSpace :: At.Parser ()
skipSimpleSpace = At.skipWhile (== ' ')

---------------------------------------------------------------------
-- Old (working)


parse :: String -> T.Start T.ParsedCommand
-- ^Parses an input string into String-command-argument-list pairs.
parse = parseCmds . preprocess

parseCmds :: [String] -> T.Start T.ParsedCommand
-- ^Parse commands to determine whether a script should be run. If a
-- script is to be run, then parse the script to command-argument
-- pairs and add a <save> command at the end.
parseCmds []              = T.Usage "This won't do anything (try: btx help).\n"
parseCmds ("in":xs)       = parseFirstIn . break ( == "and" ) $ xs
parseCmds ("help":xs)     = T.Help xs
parseCmds ("--help":xs)   = T.Help xs
parseCmds ("-h":xs)       = T.Help xs
parseCmds ("version":_)   = T.Usage "version string here"-- versionStr
parseCmds ("--version":_) = T.Usage "version string here"-- versionStr
parseCmds ("-v":_)        = T.Usage "version string here"-- versionStr
parseCmds xs              = T.Normal [] . readCmdsAndSave $ xs

parseFirstIn :: ([String], [String]) -> T.Start T.ParsedCommand
-- ^Parse the first in-command. This is necessary so we can know how
-- to load the initial working bibliography.
parseFirstIn ([],_)    = T.Usage "This won't do anything (try: btx help).\n"
parseFirstIn (_:_:_,_) = T.Usage "Command <in> allows only one argument.\n"
parseFirstIn (x:_,cs)  = T.Normal x . readCmdsAndSave $ cs

preprocess :: String -> [String]
-- ^Convert all command separators to <and> keyword and remove
-- the <and with> keyword pairs to extend argument lists.
preprocess = handleWith . words . formatTokens
    where -- Reformat to use only <and> and <with>
          formatTokens []        = []
          formatTokens (',':xs)  = " and " ++ formatTokens xs
          formatTokens ('\n':xs) = " and " ++ formatTokens xs
          formatTokens ('+':xs)  = " with " ++ formatTokens xs
          formatTokens (x:xs)    = x : formatTokens xs
          -- Remove <and with> pairs
          handleWith []                = []
          handleWith ("and":"with":xs) = handleWith xs
          handleWith (x:xs)            = x : handleWith xs

readCmdsAndSave :: [String] -> [T.ParsedCommand]
-- ^Read the individual commands in the formatted script, separate
-- into command-argument pairs and append a final <save> command.
readCmdsAndSave []          = [ ("save", []) ]
readCmdsAndSave ("and":xs)  = readCmdsAndSave xs
readCmdsAndSave (x:xs)      = (x, ys) : readCmdsAndSave zs
    where (ys,zs)  = break ( == "and" ) xs
