{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( finish
    , getScript
    , initBtx
    , runHelp
    , runBtx
    ) where

-- =============================================================== --
-- Provides the interface between the user and the model
-- =============================================================== --

import qualified Data.Text      as Tx
import qualified Types          as T
import qualified Help           as H
import Data.List                      ( intercalate         )
import System.Directory               ( listDirectory
                                      , getCurrentDirectory )
import Control.Monad.State.Lazy       ( execStateT, get
                                      , liftIO              )
import Control.Monad.Except           ( throwError
                                      , liftEither          )
import BibTeX.Parser                  ( parseBib            )
import CoreIO                         ( readOrMakeFile      )
import Formatting                     ( formatHelp
                                      , uniqueBibErr        )
import Commands                       ( hub, route, saveCmd )

-- =============================================================== --
-- Initialization

---------------------------------------------------------------------
-- Finding the script that the user wants to run

getScript :: [String] -> IO String
getScript ("run":fp:_) = readFile fp            -- Script is in a file.
getScript ("run":[])   = getContents            -- Script is from stdin.
getScript xs           = return . unwords $ xs  -- Script is from command line.

---------------------------------------------------------------------
-- Finding the working BibTeX bibliography that the user wants to use
-- and initializing the btx state with it

initBtx :: a -> FilePath -> T.ErrMonad ( a, T.BtxState )
-- ^Generate the initial state given a file path.
initBtx x [] = findUniqueBibFile >>= initBtx x
initBtx x fp = do
    content <- readOrMakeFile fp
    bib     <- liftEither . parseBib fp $ content
    return ( x, T.BtxState bib Nothing Nothing Tx.empty )

findUniqueBibFile :: T.ErrMonad FilePath
-- ^Look in current working directory for a unique .bib file.
findUniqueBibFile = do
    cwd <- liftIO getCurrentDirectory
    fps <- liftIO . listDirectory $ cwd
    case filter ( (== "bib.") . take 4 . reverse ) fps of
         (fp:[])   -> return fp
         otherwise -> throwError . uniqueBibErr $ cwd

-- =============================================================== --
-- Managers for program execution

---------------------------------------------------------------------
-- Compiling and running a btx script

runBtx :: ( [T.ParsedCommand], T.BtxState ) -> T.ErrMonad T.BtxState
-- ^Compile and run the commands an the initial state.
runBtx (cs, st) = execStateT ( compile cs [] ) st

compile :: [T.ParsedCommand] -> T.Context -> T.BtxStateMonad T.Context
-- ^Compile parsed commands (i.e., String-String list pairs) into a
-- runnable BtxStateMonad.
compile []                 rs = saveCmd [] rs
compile ( (c, args) : cs ) rs = applyCmd c args rs >>= compile cs
    where applyCmd = T.cmdCmd . route

---------------------------------------------------------------------
-- Generating help and information strings for display

runHelp :: [String] -> String
runHelp []            = formatHelp . map T.cmdSHelp $ hub
runHelp ("run":_)     = H.runHelpStr
runHelp ("all":_)     = H.allHelpStr
runHelp ("and":_)     = H.andHelpStr
runHelp (",":_)       = H.andHelpStr
runHelp ("with":_)    = H.withHelpStr
runHelp ("+":_)       = H.withHelpStr
runHelp ("copying":_) = H.copyingHelpStr
runHelp ("version":_) = H.versionHelpStr
runHelp xs            = intercalate "\n" . map ( T.cmdLHelp . route ) $ xs

-- =============================================================== --
-- Finalization

finish :: Either String T.BtxState -> IO ()
-- ^Cleanup after running a script.
finish (Left msg) = putStr msg
finish (Right _ ) = return ()
