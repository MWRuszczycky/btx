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
-- ToDo: 1. Fix the quotes parser
--       2. Write tests
--       3. Integrate back into Controller and Types
--       4. Put error messages where they belong.

-- |Starting state: Need to change how the start state is prepared
data StartUp = Help [String]
             | Usage String
             | Script (Maybe FilePath) [T.ParsedCommand]
             deriving ( Show )

parse :: String -> StartUp
parse = either err check . At.parseOnly btxParser . pack
    where err = const $ Usage cannotParseMsg

check :: StartUp -> StartUp
check (Usage xs)        = Usage xs
check (Help  xs)        = Help xs
check (Script _ [])     = Usage noCommandsMsg
check (Script _ (x:xs)) = case x of
                               ("in",[])    -> Usage invalidInCmdMsg
                               ("in",_:_:_) -> Usage invalidInCmdMsg
                               ("in",p:_)   -> Script (Just p) xs
                               otherwise    -> Script Nothing (x:xs)

-- Parse error messages
cannotParseMsg, noCommandsMsg, invalidInCmdMsg :: String
cannotParseMsg  = "Unable to parse input. (Try: btx help)\n"
noCommandsMsg   = "This won't do anything. (Try: btx help)\n"
invalidInCmdMsg = "Invalid argument for <in>. (Try: btx help in)\n"

---------------------------------------------------------------------
-- Input parser

btxParser :: At.Parser StartUp
btxParser = do
    At.skipSpace
    At.choice [ cryForHelp, versionRequest, btxScript ]

cryForHelp :: At.Parser StartUp
cryForHelp = do
    At.choice [ At.string "help", At.string "--help", At.string "-h" ]
    At.takeText >>= pure . Help . words . unpack

versionRequest :: At.Parser StartUp
versionRequest = do
    At.choice [ At.string "version", At.string "--version", At.string "-v" ]
    pure . Usage $ "version string here" -- versionStr

btxScript :: At.Parser StartUp
btxScript = At.many' aToken >>= pure . Script Nothing . toCommands

toCommands :: [String] -> [T.ParsedCommand]
-- ^Read the individual commands in the formatted script, handle
-- 'with and' constructions, separate into command-argument pairs and
-- append a final <save> command.
toCommands []       = [ ("save", []) ]
toCommands (",":xs) = toCommands xs
toCommands (x:xs)   = case break ( flip elem [",", "+"] ) xs of
                           (ys, "+":zs)     -> toCommands $ x : (ys ++ zs)
                           (ys, ",":"+":zs) -> toCommands $ x : (ys ++ zs)
                           (ys, zs)         -> (x, ys) : toCommands zs

aToken :: At.Parser String
aToken = do
    At.skipWhile (== ' ')
    At.choice [ withKey, andKey, aWord, quotedString ]

aWord :: At.Parser String
aWord = At.many1' $ At.satisfy (At.notInClass "+, \n\r'\"")

quotedString :: At.Parser String
-- !!! Needs to be fixed: internal '\"' will not be recognized !!!
quotedString = do
    open <- At.char '\"' <|> At.char '\''
    At.manyTill At.anyChar (At.char open)

withKey :: At.Parser String
withKey = At.choice [ At.char   '+'    *> pure "+"
                    , At.string "with" *> pure "+"
                    ]

andKey :: At.Parser String
andKey  = At.choice [ At.char   ','    *> pure ","
                    , At.string "and"  *> pure ","
                    , At.char   '\n'   *> pure ","
                    , At.string "\n\r" *> pure ","
                    ]

---------------------------------------------------------------------

-- =============================================================== --
-- Old (working)


-- parse :: String -> T.Start T.ParsedCommand
-- -- ^Parses an input string into String-command-argument-list pairs.
-- parse = parseCmds . preprocess
--
-- parseCmds :: [String] -> T.Start T.ParsedCommand
-- -- ^Parse commands to determine whether a script should be run. If a
-- -- script is to be run, then parse the script to command-argument
-- -- pairs and add a <save> command at the end.
-- parseCmds []              = T.Usage "This won't do anything (try: btx help).\n"
-- parseCmds ("in":xs)       = parseFirstIn . break ( == "and" ) $ xs
-- parseCmds ("help":xs)     = T.Help xs
-- parseCmds ("--help":xs)   = T.Help xs
-- parseCmds ("-h":xs)       = T.Help xs
-- parseCmds ("version":_)   = T.Usage "version string here"-- versionStr
-- parseCmds ("--version":_) = T.Usage "version string here"-- versionStr
-- parseCmds ("-v":_)        = T.Usage "version string here"-- versionStr
-- parseCmds xs              = T.Normal [] . readCmdsAndSave $ xs
--
-- parseFirstIn :: ([String], [String]) -> T.Start T.ParsedCommand
-- -- ^Parse the first in-command. This is necessary so we can know how
-- -- to load the initial working bibliography.
-- parseFirstIn ([],_)    = T.Usage "This won't do anything (try: btx help).\n"
-- parseFirstIn (_:_:_,_) = T.Usage "Command <in> allows only one argument.\n"
-- parseFirstIn (x:_,cs)  = T.Normal x . readCmdsAndSave $ cs
--
-- preprocess :: String -> [String]
-- -- ^Convert all command separators to <and> keyword and remove
-- -- the <and with> keyword pairs to extend argument lists.
-- preprocess = handleWith . words . formatTokens
--     where -- Reformat to use only <and> and <with>
--           formatTokens []        = []
--           formatTokens (',':xs)  = " and " ++ formatTokens xs
--           formatTokens ('\n':xs) = " and " ++ formatTokens xs
--           formatTokens ('+':xs)  = " with " ++ formatTokens xs
--           formatTokens (x:xs)    = x : formatTokens xs
--           -- Remove <and with> pairs
--           handleWith []                = []
--           handleWith ("and":"with":xs) = handleWith xs
--           handleWith (x:xs)            = x : handleWith xs
--
-- readCmdsAndSave :: [String] -> [T.ParsedCommand]
-- -- ^Read the individual commands in the formatted script, separate
-- -- into command-argument pairs and append a final <save> command.
-- readCmdsAndSave []          = [ ("save", []) ]
-- readCmdsAndSave ("and":xs)  = readCmdsAndSave xs
-- readCmdsAndSave (x:xs)      = (x, ys) : readCmdsAndSave zs
--     where (ys,zs)  = break ( == "and" ) xs
