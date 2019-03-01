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
import Control.Applicative                    ( (<|>), liftA2
                                              , many, some       )
import Control.Monad                          ( guard            )
import Data.Char                              ( isSpace          )
import Data.Text                              ( Text
                                              , pack
                                              , unpack           )
import Model.Core.Messages.Help               ( invalidUsageErr
                                              , noCommandsErr
                                              , unableToParseErr
                                              , versionStr       )

---------------------------------------------------------------------
-- Exported btx parser interface

parse :: Either String Text -> T.Start
parse (Left  x) = T.Usage x
parse (Right x) = either err check . At.parseOnly btxParser $ x
    where err = const $ T.Usage unableToParseErr

check :: T.Start -> T.Start
-- ^Check result of btx input parsing. In particular, make sure the
-- first <in> command, if used, is used correctly. This is necessary
-- to ensure proper loading of the *initial* working bibliography.
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
    pure . T.Usage $ versionStr

---------------------------------------------------------------------
-- Script parsing

btxScript :: At.Parser T.Start
btxScript = many aToken >>= pure . T.Script Nothing . toCommands

toCommands :: [String] -> [T.ParsedCommand]
-- ^Format commands from the parsed script:
-- 1. Remove any empty strings.
-- 2. Remove ', +' (i.e., <and with>) and read through argements.
-- 3. Remove isolated '+' (i.e., <with>) and read through arguments.
-- 5. Parse individual commands on ',' (i.e., <and>).
-- 6. Append a final <save> command.
toCommands []       = [ ("save", []) ]
toCommands ("":xs ) = toCommands xs
toCommands (",":xs) = toCommands xs
toCommands ("+":xs) = toCommands xs
toCommands (x:xs  ) = case break ( flip elem [",", "+", ""] ) xs of
                           (ys, "":zs     ) -> toCommands $ x : (ys ++ zs)
                           (ys, "+":zs    ) -> toCommands $ x : (ys ++ zs)
                           (ys, ",":"+":zs) -> toCommands $ x : (ys ++ zs)
                           (ys, zs        ) -> (x, ys) : toCommands zs

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
