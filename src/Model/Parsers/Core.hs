{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Core
    (
    ) where

import qualified Data.Attoparsec.Text   as At
import qualified Model.Core.Types       as T
import qualified Data.Text              as Tx
import           Data.Text                    ( Text          )
import           Control.Applicative          ( (<|>), liftA2
                                              , many, some    )
