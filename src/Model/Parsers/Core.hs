{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Core
    ( comment
    , comments
    , quotedTxt
    , quotedStr
    ) where

import qualified Data.Attoparsec.Text   as At
import qualified Model.Core.Types       as T
import qualified Data.Text              as Tx
import           Data.Text                    ( Text          )
import           Control.Applicative          ( (<|>), liftA2
                                              , many, some    )

comment :: At.Parser ()
comment = At.char '#' *> At.skipWhile (not . At.isEndOfLine) *> At.endOfLine

comments :: At.Parser ()
comments = many (At.skipSpace *> comment) *> At.skipSpace

quotedStr :: At.Parser String
quotedStr = Tx.unpack <$> quotedTxt

quotedTxt :: At.Parser Text
quotedTxt = do
    open <- At.char '\"' <|> At.char '\''
    quotedContent (Tx.singleton open)

quotedContent :: Text -> At.Parser Text
quotedContent c = escaped <|> closeQuote <|> moreContent
    where escaped     = At.string ("\\" <> c) *> fmap (c <>) (quotedContent c)
          closeQuote  = At.string c           *> pure Tx.empty
          moreContent = liftA2 Tx.cons At.anyChar (quotedContent c)
