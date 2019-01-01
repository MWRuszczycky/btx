module Model.Core.ScriptParser
    ( parse
    ) where

-- =============================================================== --
-- Module for parsing btx scripts
-- =============================================================== --

import qualified Model.Core.Types as T
import Model.Core.Messages.Help         ( versionStr )

parse :: String -> T.Start T.ParsedCommand
-- ^Parses an input string into String-command-argument-list pairs.
parse = parseCmds . preprocess

parseCmds :: [String] -> T.Start T.ParsedCommand
-- ^Parse commands to determine whether a script should be run. If a
-- script is to be run, then parse the script to command-argument
-- pairs and add a <save> command at the end.
parseCmds []              = T.Usage "This won't do anything (try: btx help)."
parseCmds ("in":xs)       = parseFirstIn . break ( == "and" ) $ xs
parseCmds ("help":xs)     = T.Help xs
parseCmds ("--help":xs)   = T.Help xs
parseCmds ("-h":xs)       = T.Help xs
parseCmds ("version":_)   = T.Usage versionStr
parseCmds ("--version":_) = T.Usage versionStr
parseCmds ("-v":_)        = T.Usage versionStr
parseCmds xs              = T.Normal [] . readCmdsAndSave $ xs

parseFirstIn :: ([String], [String]) -> T.Start T.ParsedCommand
-- ^Parse the first in-command. This is necessary so we can know how
-- to load the initial working bibliography.
parseFirstIn ([],_)    = T.Usage "This won't do anything (try: btx help)."
parseFirstIn (_:_:_,_) = T.Usage "Command <in> allows only one argument."
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
