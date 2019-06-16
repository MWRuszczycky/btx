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
import Data.Version                           ( showVersion      )
import Paths_btx                              ( version          )
import Control.Applicative                    ( (<|>), liftA2
                                              , many, some       )
import Data.Text                              ( Text, unpack     )
import Model.Core.Messages.Help               ( invalidUsageErr
                                              , noCommandsErr
                                              , unableToParseErr )

---------------------------------------------------------------------
-- Exported btx parser interface

parse :: Text -> T.Start
parse = either err id . At.parseOnly btxParser
    where err = const $ T.Usage unableToParseErr

---------------------------------------------------------------------
-- Parser entry

btxParser :: At.Parser T.Start
btxParser = do
    At.skipSpace
    result <- At.choice [ cryForHelp, versionRequest, btxScript ]
    At.endOfInput
    pure result

---------------------------------------------------------------------
-- Help and version parsing

cryForHelp :: At.Parser T.Start
cryForHelp = do
    At.choice [ At.string "help", At.string "--help", At.string "-h" ]
    At.takeText >>= pure . T.Help . words . unpack

versionRequest :: At.Parser T.Start
versionRequest = do
    At.choice [ At.string "version", At.string "--version", At.string "-v" ]
    pure . T.Usage $ "btx version " ++ showVersion version ++ "\n"

---------------------------------------------------------------------
-- Script parsing

btxScript :: At.Parser T.Start
btxScript = many aToken >>= pure . finalize . format

finalize :: [T.ParsedCommand] -> T.Start
-- ^Finalize the parse by identifying the initial bibliography file.
finalize []                = T.Usage   noCommandsErr
finalize (("in",[])   :_)  = T.Usage $ invalidUsageErr "in"
finalize (("in",_:_:_):_)  = T.Usage $ invalidUsageErr "in"
finalize (("in",p:_)  :xs) = T.Script (Just p) xs
finalize xs                = T.Script Nothing  xs

format :: [String] -> [T.ParsedCommand]
-- ^Format commands from the parsed script:
-- 1. Remove any empty strings.
-- 2. Remove ', +' (i.e., <and with>) and read through argements.
-- 3. Remove isolated '+' (i.e., <with>) and read through arguments.
-- 4. Parse individual commands on ',' (i.e., <and>).
-- 5. Append a final <save> command.
format []       = [ ("save", []) ]
format ("" :xs) = format xs
format (",":xs) = format xs
format ("+":xs) = format xs
format (x  :xs) = case break ( flip elem [",", "+", ""] ) xs of
                             (ys, "":zs     ) -> format $ x : (ys ++ zs)
                             (ys, "+":zs    ) -> format $ x : (ys ++ zs)
                             (ys, ",":"+":zs) -> format $ x : (ys ++ zs)
                             (ys, zs        ) -> (x, ys) : format zs

aToken :: At.Parser String
aToken = do
    At.skipWhile (== ' ')
    At.choice [ withKey, andKey, aWord, quotedString ]

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

aWord :: At.Parser String
aWord = some $ At.satisfy (At.notInClass "+, \n\r'\"")

quotedString :: At.Parser String
quotedString = do
    open <- At.char '\"' <|> At.char '\''
    unpack <$> quotedContent (Tx.singleton open)

quotedContent :: Text -> At.Parser Text
quotedContent c = escaped <|> closeQuote <|> moreContent
    where escaped     = At.string ("\\" <> c) *> fmap (c <>) (quotedContent c)
          closeQuote  = At.string c           *> pure Tx.empty
          moreContent = liftA2 Tx.cons At.anyChar (quotedContent c)
