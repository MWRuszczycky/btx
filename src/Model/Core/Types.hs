{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Types
    ( -- State
      BtxState          (..)
    , BtxMonad
    , ErrMonad
    , ErrString
    , ViewMonad
    , Config            (..)
    , defaultConfig
    , Directive         (..)
    , Configurator
    , Option
    -- StyleMaps
    , StyleMap
    -- Bibliographies
    , Bibliography      (..)
    , Context
    , Entry             (..)
    , Field
    , Key
    , Ref               (..)
    , References
    -- Commands
    , Command           (..)
    , CommandMonad
    , ParsedCommand
    , HelpInfo          (..)
    ) where

import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Tx
import           Data.Text                       ( Text    )
import           Control.Monad.State.Lazy        ( StateT  )
import           Control.Monad.Except            ( ExceptT )
import           Control.Monad.Reader            ( Reader  )
import           Control.Monad.Writer            ( WriterT )
import           Data.Monoid                     ( Endo    )


-- =============================================================== -- 
-- State

type ErrString = String

-- |Monad for handling exceptions in the IO Monad.
-- ErrMonad a = IO ( Either ErrString a )
type ErrMonad = ExceptT ErrString IO

-- |Transformer stack for managing program state in the IO Monad.
-- BtxMonad a = IO ( Either ErrString ( State BtxState a ) )
type BtxMonad = StateT BtxState ErrMonad

-- |Program state.
data BtxState = BtxState {
      inBib    :: Bibliography       -- Current bibliography
    , toBib    :: Maybe Bibliography -- Target bibliography
    , fromBib  :: Maybe Bibliography -- Source bibliography
    , logger   :: Text               -- Log of actions performed
    , config   :: Config             -- Configuration
    }

-- The ViewMonad is a transformer stack with a Writer monad stacked
-- on an inner Reader monad. The Reader monad provides access to the
-- program configuration (e.g., to determine display styles). The
-- Monoid for the Writer monad is composition of functions over lists
-- of text blocks that are being composed together.
type ViewMonad = WriterT (Endo [Text]) (Reader Config)

-- |Configuration
data Config = Config {
      cDirective :: Directive
    , cStyles    :: StyleMap
    , cUseANSI   :: Bool
    }

defaultConfig :: Config
defaultConfig = Config {
      cDirective = Script Tx.empty
    , cStyles    = Map.empty
    , cUseANSI   = True
    }

data Directive =
      Help     [String]
    | RunFile  FilePath
    | RunStdIn
    | Script   Text
    | Version
      deriving (Eq, Show)

type Configurator = Config -> ErrMonad Config

type Option = (String, [String])

-- =============================================================== -- 
-- Style maps for holding color/display preferences

type StyleMap = Map.Map Text (Text -> Text)

-- =============================================================== -- 
-- Bibliographies

-- |Representation of a BibTeX bibliography
data Bibliography = Bibliography {
      path   :: FilePath      -- Path to the .bib BibTeX file
    , refs   :: References    -- Encoded references from the .bib file
    , header :: Text          -- Header metadata associated with the .bib file
    } deriving ( Eq, Show )

-- |Representation of an individual BibTeX entry
-- Comments are only recognized if they immediately follow the entry.
data Entry = Entry {
      theType  :: Text        -- Entry type (e.g., article, book, ..)
    , fields   :: [ Field ]   -- Entry fields (e.g., authors, title ..)
    , metadata :: [ Text  ]   -- Entry metadata
    } deriving ( Show, Eq )

-- |Representation of a bibliography entry in the context.
data Ref = Ref FilePath Key Entry
         | Missing FilePath Key ErrString

type Key = Text

-- |Representation of a parsed BibTeX .bib file.
type References = Map.Map Key Entry

type Field = ( Text, Text )

type Context = [Ref]

-- =============================================================== -- 
-- Commands

-- |Monadic Btx command that can be run in the BtxMonad
-- The a parameter is used for passing arguments between commands.
-- This parameter is usually instantiated as either () or [Ref] so
-- that lists of bibliography entries can be passed between commands.
type CommandMonad a = [String] -> a -> BtxMonad a

-- |Pair of the user command input and argument list.
type ParsedCommand = (String, [String])

-- |Generalized Btx command that includes the monadic component and
-- additional information including arguments and help strings.
data Command a = Command {
      cmdName :: String         -- User interface to the command
    , cmdCmd  :: CommandMonad a -- Monadic Btx command
    , cmdHelp :: HelpInfo       -- Help information for the command
    }

instance Show ( Command a ) where
    show = cmdName

-- |General data type for managing help information about commands,
-- keywords and directives.
data HelpInfo = HelpInfo {
      names     :: [Text]
    , usage     :: Text
    , shortHelp :: Text
    , longHelp  :: Text
    }
