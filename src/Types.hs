{-# LANGUAGE OverloadedStrings #-}

module Types
    ( -- State
      BtxState          (..)
    , BtxStateMonad     (..)
    , ErrMonad          (..)
    , ErrString         (..)
    , Start             (..)
    -- Bibliographies
    , Bibliography      (..)
    , Entry             (..)
    , Field             (..)
    , Ref               (..)
    , References        (..)
    , Context           (..)
    -- Commands
    , Command           (..)
    , CommandArgsMonad  (..)
    , CommandMonad      (..)
    ) where

import qualified Data.Map.Strict as Map
import Data.Text                        ( Text, unpack  )
import Data.List                        ( intercalate   )
import Control.Monad.State.Lazy         ( StateT        )
import Control.Monad.Except             ( ExceptT       )

---------------------------------------------------------------------
-- State

type ErrString = String

-- |Monad for handling exceptions in the IO Monad.
-- ErrMonad a = IO ( Either ErrString a )
type ErrMonad = ExceptT ErrString IO

-- |Transformer stack for managing program state in the IO Monad.
-- BtxStateMonad a = IO ( Either ErrString ( State BtxState a ) )
type BtxStateMonad = StateT BtxState ErrMonad

-- |Program state.
data BtxState = BtxState {
      inBib   :: Bibliography       -- Current bibliography
    , toBib   :: Maybe Bibliography -- Target bibliography
    , fromBib :: Maybe Bibliography -- Source bibliography
    , logger  :: Text               -- Log of action performed
    }

-- |Starting state
data Start a = Help [String]
             | Usage String
             | Normal FilePath [a]

---------------------------------------------------------------------
-- Bibliographies

-- |Programmatic representation of a BibTeX bibliography
data Bibliography = Bibliography {
      path :: FilePath      -- Path to the .bib BibTeX file
    , refs :: References    -- Encoded references from the .bib file
    } deriving ( Eq, Show )

-- |Programmatic representation of an individual BibTeX entry
-- Comments are only recognized if they immediately follow the entry.
data Entry = Entry {
      theType  :: Text        -- Entry type (e.g., article, book, ..)
    , fields   :: [ Field ]   -- Entry fields (e.g., authors, title ..)
    , comments :: [ Text  ]   -- Entry comments
    } deriving ( Show, Eq )

-- |Programmatic representation of a parsed BibTeX .bib file.
type References = Map.Map Text Entry

type Field = ( Text, Text )

type Ref = ( Text, Entry )

type Context = [Ref]

---------------------------------------------------------------------
-- Commands

-- |Monadic Btx command that can be run in the BtxStateMonad
-- The a parameter is used for passing arguments between commands.
-- This parameter is usually instantiated as either () or [Ref] so
-- that lists of bibliography entries can be passed between commands.
type CommandMonad a = [String] -> a -> BtxStateMonad a

type CommandArgsMonad a = a -> BtxStateMonad a

-- |Generalized Btx command that includes the monadic component and
-- additional information including arguments and help strings.
data Command a = Command {
      cmdName  :: String         -- User interface to the command
    , cmdCmd   :: CommandMonad a -- Monadic Btx command
    , cmdSHelp :: String         -- Short help information
    , cmdLHelp :: String         -- Long help information
    }

instance Show ( Command a ) where
    show = cmdName
