{-# LANGUAGE OverloadedStrings #-}

module Model.Core.ScriptParser
    ( parse
    ) where

-- =============================================================== --
-- Module for parsing btx scripts
-- =============================================================== --

import qualified Data.Attoparsec.Text   as At
import qualified Data.Text              as Tx
import qualified Model.Core.Types       as T
import Control.Applicative                    ( (<|>)            )
import Control.Monad                          ( guard            )
import Data.Char                              ( isSpace          )
import Data.Text                              ( Text
                                              , pack
                                              , unpack           )
import Model.Core.Messages.Help               ( invalidUsageErr
                                              , noCommandsErr
                                              , unableToParseErr
                                              , versionStr       )

-- ******************************** --
-- ToDo: 1. Fix the quotes parser
--       2. Write tests
-- ******************************** --

---------------------------------------------------------------------
-- Exported parser interface

parse :: Either String String -> T.Start
parse (Left  x) = T.Usage x
parse (Right x) = either err check . At.parseOnly btxParser . pack $ x
    where err = const $ T.Usage unableToParseErr

check :: T.Start -> T.Start
check (T.Usage xs)        = T.Usage xs
check (T.Help  xs)        = T.Help xs
check (T.Script _ [])     = T.Usage noCommandsErr
check (T.Script _ (x:xs)) = case x of
                                 ("in",[])    -> T.Usage $ invalidUsageErr "in"
                                 ("in",_:_:_) -> T.Usage $ invalidUsageErr "in"
                                 ("in",p:_)   -> T.Script (Just p) xs
                                 otherwise    -> T.Script Nothing (x:xs)

---------------------------------------------------------------------
-- Parser entry

btxParser :: At.Parser T.Start
btxParser = do
    At.skipSpace
    At.choice [ cryForHelp, versionRequest, btxScript ]

---------------------------------------------------------------------
-- Help and version parsing

cryForHelp :: At.Parser T.Start
cryForHelp = do
    At.choice [ At.string "help", At.string "--help", At.string "-h" ]
    At.takeText >>= pure . T.Help . words . unpack

versionRequest :: At.Parser T.Start
versionRequest = do
    At.choice [ At.string "version", At.string "--version", At.string "-v" ]
    pure . T.Usage $ versionStr

---------------------------------------------------------------------
-- Script parsing

btxScript :: At.Parser T.Start
btxScript = At.many' aToken >>= pure . T.Script Nothing . toCommands

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
