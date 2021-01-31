{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Model.Parsers.Config
    ( formatInput
    , parseConfig
    , parseConfigTxt
    , parseInput
    , parseScript
    ) where

import qualified Data.Attoparsec.Text as At
import qualified Model.Parsers.Core   as P
import qualified Model.Core.Types     as T
import qualified Data.Text            as Tx
import           Data.Text                  ( Text                )
import           Data.Char                  ( isAlphaNum, isSpace )
import           Control.Monad.Except       ( throwError          )
import           Control.Applicative        ( (<|>), many, some   )

-- =============================================================== -- 
-- Formatting input

formatInput :: [String] -> Text
formatInput = Tx.unwords . map Tx.pack

-- =============================================================== -- 
-- Parsers

parseInput :: Text -> Either T.ErrString [T.Configurator]
parseInput txt = At.parseOnly foo txt >>= bar
    where bar d = pure [ \ c -> pure $ c { T.cDirective = d } ]
          foo   = do At.skipSpace
                     dir <- directive
                     At.endOfInput
                     pure dir

parseConfig :: Text -> Either T.ErrString [T.Configurator]
parseConfig txt = map readKeyVal <$> parseConfigTxt txt

parseScript :: Text -> Either T.ErrString (Maybe FilePath, [T.ParsedCommand])
parseScript txt = At.parseOnly commands txt >>= readFirstInBib

-- ------------------------------------------------------------------
-- Parsing the command line input

directive :: At.Parser T.Directive
directive = At.choice [ helpDirective
                      , versionDirective
                      , runDirective
                      , scriptDirective
                      ]

helpDirective :: At.Parser T.Directive
helpDirective = do
    At.choice [ At.string "help", At.string "--help", At.string "-h" ]
    T.Help . words . Tx.unpack <$> At.takeText

versionDirective :: At.Parser T.Directive
versionDirective = do
    At.choice [ At.string "version", At.string "--version", At.string "-v" ]
    pure T.Version

runDirective :: At.Parser T.Directive
runDirective = do
    At.string "run"
    ( some (At.satisfy isSpace) *> pure () ) <|> At.endOfInput
    path <- Tx.strip <$> At.takeText
    if Tx.null path
       then pure   T.RunStdIn
       else pure . T.RunFile . Tx.unpack $ path

scriptDirective :: At.Parser T.Directive
scriptDirective = At.takeText >>= pure . T.Script . Tx.strip

---------------------------------------------------------------------
-- Parsing scripts

commands :: At.Parser [T.ParsedCommand]
-- ^Parse text script into parsed command-argument pairs
-- 1. Remove any empty strings.
-- 2. Parse individual commands on ',' (i.e., <and>).
-- 3. Append a final <save> command.
commands = ( many aToken <* At.endOfInput ) >>= pure . go
    where go []       = [ ("save", []) ]
          go ("" :xs) = go xs
          go (",":xs) = go xs
          go (x  :xs) = case break ( flip elem [",", ""] ) xs of
                             (ys, "":zs) -> go $ x  : (ys <> zs)
                             (ys,    zs) -> (x, ys) : go zs

aToken :: At.Parser String
aToken = At.choice [ andKey, aWord, P.quotedStr ] <* At.skipSpace
    where aWord  = some $ At.satisfy (At.notInClass ", \n\r'\"")
          andKey = At.choice [ At.char   ','    *> pure ","
                             , At.string "and"  *> pure ","
                             ]

readFirstInBib :: [T.ParsedCommand]
                  -> Either T.ErrString (Maybe FilePath, [T.ParsedCommand])
readFirstInBib (("in",[])   : _) = Left "Invalid use of <in>"
readFirstInBib (("in",_:_:_): _) = Left "Invalid use of <in>"
readFirstInBib (("in",p:_)  :xs) = pure ( Just p, xs  )
readFirstInBib xs                = pure ( Nothing, xs )

---------------------------------------------------------------------
-- Parsing configuration files
-- Configuration files use a simple key:value format, where comments
-- begin with a '#'. The value runs from the first nonspace character
-- after the ':' until the end of the line or a comment. Any leading
-- or trailing space of the value is stripped; however, any internal
-- spaces are left unchanged.

parseConfigTxt :: Text -> Either T.ErrString [(Text,Text)]
parseConfigTxt = At.parseOnly configKeyValPairs

configKeyValPairs :: At.Parser [(Text, Text)]
configKeyValPairs = do
    P.comments
    xs <- many keyValPair
    P.comments
    At.endOfInput
    pure xs

validKey :: At.Parser Text
validKey = At.takeWhile1 $ \ c -> isAlphaNum c || c == '-' || c == '_'

validValue :: At.Parser Text
validValue = fmap Tx.strip . At.takeWhile1 . At.notInClass $ ":#\n\r\t"

keyValPair :: At.Parser (Text, Text)
keyValPair = do
    P.comments
    key <- validKey
    P.comments
    At.char ':'
    P.comments
    val <- validValue
    P.comment <|> At.endOfLine
    pure (key, val)

-- =============================================================== -- 
-- Converting parsed values to configurators

---------------------------------------------------------------------
-- Configuration key-value pairs

readKeyVal :: (Text, Text) -> T.Configurator
readKeyVal ("color",x) = \ c ->
    readFlag x >>= \ y -> pure $ c { T.cUseANSI = y }
readKeyVal (_,_)       = \ c -> pure c

readFlag :: Text -> T.ErrMonad Bool
readFlag "yes"   = pure True
readFlag "true"  = pure True
readFlag "no"    = pure False
readFlag "false" = pure False
readFlag x       = throwError errMsg
    where errMsg = concat [ "Invalid flag value: "
                          , Tx.unpack x
                          , ", expected: 'yes', 'true', 'no' or 'false'"
                          ]
