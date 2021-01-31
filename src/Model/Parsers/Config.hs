{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Model.Parsers.Config
    ( formatInput
    , parseConfigTxt
    , parseInput
    , parseScript
    ) where

import qualified Data.Attoparsec.Text   as At
import qualified Model.Core.ErrMonad    as E
import qualified Model.Core.Types       as T
import qualified Model.Parsers.Core     as P
import qualified Data.Text              as Tx
import           Data.Text                    ( Text          )
import           Data.Char                    ( isAlphaNum    )
import           Control.Monad.Except         ( throwError
                                              , liftIO        )
import           Control.Applicative          ( (<|>), liftA2
                                              , many, some    )

-- =============================================================== -- 
-- Formatting input

formatInput :: [String] -> Text
formatInput = Tx.unwords . map Tx.pack

-- =============================================================== -- 
-- Parsers

parseInput :: Text -> Either T.ErrString [T.Configurator]
parseInput txt = do
    (opts, script) <- At.parseOnly parseCmdLine txt
    pure $ configScript script : map readOption opts

parseCmdLine :: At.Parser ([T.Option], Text)
parseCmdLine = do
    At.skipSpace
    flags <- many $ At.choice [ helpOpt, versionOpt, runOpt ]
    rest <- At.takeText
    At.endOfInput
    pure (flags, rest)

-- ------------------------------------------------------------------
-- Parsing Options

helpOpt :: At.Parser T.Option
helpOpt = do
    At.choice [ At.string "help", At.string "--help", At.string "-h" ]
    (,) "help" . words . Tx.unpack <$> At.takeText

versionOpt :: At.Parser T.Option
versionOpt = do
    At.choice [ At.string "version", At.string "--version", At.string "-v" ]
    pure ("version", [])

runOpt :: At.Parser T.Option
runOpt = do
    At.string "run"
    (,) "run" . words . Tx.unpack <$> At.takeText

---------------------------------------------------------------------
-- Parsing scripts

parseScript :: Text -> Either T.ErrString (Maybe FilePath, [T.ParsedCommand])
parseScript txt = At.parseOnly commands txt >>= readCmds

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
-- Scripts

readCmds :: [T.ParsedCommand]
            -> Either T.ErrString (Maybe FilePath, [T.ParsedCommand])
readCmds []                = Left   noCommandsErr
readCmds (("in",[])   :_)  = Left $ invalidUsageErr "in"
readCmds (("in",_:_:_):_)  = Left $ invalidUsageErr "in"
readCmds (("in",p:_)  :xs) = pure ( Just p, xs  )
readCmds xs                = pure ( Nothing, xs )

configScript :: Text -> T.Configurator
configScript xs = \ c -> pure c { T.cScript = xs }

-- ------------------------------------------------------------------
-- Options

readOption :: T.Option -> T.Configurator
readOption ("help",   []) = \ c -> pure $ c { T.cHelp = ["btx"]     }
readOption ("help",   xs) = \ c -> pure $ c { T.cHelp = xs          }
readOption ("version", _) = \ c -> pure $ c { T.cHelp = ["version"] }
readOption ("run",    xs) = configRun xs
readOption (x,         _) = \ _ -> throwError $ "Unknown option: " <> x

configRun :: [FilePath] -> T.Configurator
configRun []      config = do script <- Tx.pack <$> liftIO getContents
                              pure $ config { T.cScript = script }
configRun (fp:[]) config = do script <- E.readFileExcept fp
                              pure $ config { T.cScript = script }
configRun _       _      = throwError "run takes at most one argument"

-- =============================================================== -- 
-- Error messages

invalidUsageErr :: String -> T.ErrString
invalidUsageErr c = unwords [ "Invalid usage for command <" ++ c ++ ">."
                            , "(Try: btx help " ++ c ++ ")\n"
                            ]

noCommandsErr :: T.ErrString
noCommandsErr   = unwords [ "This won't do anything --"
                          , "a script or command needs to be provided."
                          , "(Try: btx help)\n"
                          ]
