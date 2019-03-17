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

import qualified Data.Text                   as Tx
import qualified Data.Text.IO                as Tx
import qualified Model.Core.Types            as T
import qualified Model.Core.Messages.Help    as H
import qualified Model.Core.Messages.Copying as H
import Data.Text                                    ( Text, pack          )
import Data.List                                    ( intercalate         )
import System.Directory                             ( listDirectory
                                                    , getCurrentDirectory )
import Control.Monad.State.Lazy                     ( execStateT, liftIO  )
import Control.Monad.Except                         ( throwError
                                                    , liftEither          )
import Model.BibTeX.Parser                          ( parseBib            )
import Model.CoreIO.ErrMonad                        ( readOrMakeFile      )
import Commands                                     ( hub, route,         )

-- =============================================================== --
-- Initialization

---------------------------------------------------------------------
-- Finding the script that the user wants to run

getScript :: [String] -> IO (Either String Text)
getScript ("run":fp:_) = Right <$> Tx.readFile fp
getScript ("run":[])   = pure . Left $ H.missingScriptErr
getScript xs           = pure . Right . pack . unwords $ xs

---------------------------------------------------------------------
-- Finding the working BibTeX bibliography that the user wants to use
-- and initializing the btx state with it

initBtx :: Maybe FilePath -> T.ErrMonad T.BtxState
-- ^Generate the initial state given a file path.
initBtx Nothing   = findUniqueBibFile >>= initBtx . Just
initBtx (Just fp) = do
    content <- readOrMakeFile fp
    bib     <- liftEither . parseBib fp $ content
    pure $ T.BtxState bib Nothing Nothing Tx.empty

findUniqueBibFile :: T.ErrMonad FilePath
-- ^Look in current working directory for a unique .bib file.
findUniqueBibFile = do
    cwd <- liftIO getCurrentDirectory
    fps <- liftIO . listDirectory $ cwd
    case filter ( (== "bib.") . take 4 . reverse ) fps of
         (fp:[]) -> pure fp
         _       -> throwError . H.uniqueBibErr $ cwd

-- =============================================================== --
-- Managers for program execution

---------------------------------------------------------------------
-- Compiling and running a btx script

runBtx :: [T.ParsedCommand] -> T.BtxState -> T.ErrMonad T.BtxState
-- ^Compile and run the commands an the initial state.
runBtx cs = execStateT $ compile cs []

compile :: [T.ParsedCommand] -> T.Context -> T.BtxStateMonad T.Context
-- ^Compile parsed commands (i.e., String-String list pairs) into a
-- runnable BtxStateMonad.
compile []                 rs = pure rs
compile ( (c, args) : cs ) rs = applyCmd c args rs >>= compile cs
    where applyCmd = T.cmdCmd . route

---------------------------------------------------------------------
-- Generating help and information strings for display

runHelp :: [String] -> String
runHelp []            = H.displayHelp hub
runHelp ("run":_)     = H.runHelpStr
runHelp ("all":_)     = H.allHelpStr
runHelp ("and":_)     = H.andHelpStr
runHelp (",":_)       = H.andHelpStr
runHelp ("with":_)    = H.withHelpStr
runHelp ("+":_)       = H.withHelpStr
runHelp ("copying":_) = H.copyingStr
runHelp ("version":_) = H.versionHelpStr
runHelp xs            = intercalate "\n" . map ( T.cmdLHelp . route ) $ xs

-- =============================================================== --
-- Finalization

finish :: Either String T.BtxState -> IO ()
-- ^Cleanup after running a script.
finish (Left msg) = putStr msg
finish (Right bs)
    | Tx.null lg = pure ()
    | otherwise  = Tx.putStrLn lg
    where lg = T.logger bs
