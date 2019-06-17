{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( finish
    , getInput
    , getStyleMap
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
import System.IO                                    ( stdout
                                                    , hIsTerminalDevice   )
import Control.Monad.State.Lazy                     ( execStateT
                                                    , foldM, liftIO       )
import Control.Monad.Except                         ( throwError
                                                    , liftEither          )
import Model.BibTeX.Parser                          ( parseBib            )
import Model.CoreIO.ErrMonad                        ( readOrMakeFile      )
import Model.Core.Formatting                        ( noStyles
                                                    , defaultStyles       )
import Commands                                     ( hub, route,         )

-- =============================================================== --
-- Initialization

---------------------------------------------------------------------
-- Finding the script or directive that the user wants to run

getInput :: [String] -> IO (Either String Text)
getInput ("run":fp:_) = Right <$> Tx.readFile fp
getInput ("run":[])   = pure . Left $ H.missingScriptErr
getInput xs           = pure . Right . pack . unwords $ xs

---------------------------------------------------------------------
-- Determine which style map should be used. If stdout is a terminal
-- then a colored style map should be used. Otherwise, a plain style
-- map should be used so that escape sequences are not inserted.

getStyleMap :: IO T.StyleMap
getStyleMap = do
    useStyles <- hIsTerminalDevice stdout
    if useStyles
       then pure defaultStyles
       else pure noStyles

---------------------------------------------------------------------
-- Finding the working BibTeX bibliography that the user wants to use
-- and initializing the btx state with it

initBtx :: T.StyleMap -> Maybe FilePath -> T.ErrMonad T.BtxState
-- ^Generate the initial state given a file path.
initBtx sm Nothing   = findUniqueBibFile >>= initBtx sm . Just
initBtx sm (Just fp) = do
    content <- readOrMakeFile fp
    bib     <- liftEither . parseBib fp $ content
    pure $ T.BtxState bib Nothing Nothing Tx.empty sm

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
-- ^Compile and run the commands on the initial state.
runBtx = execStateT . foldM runCmd []
    where runCmd = flip . uncurry $ T.cmdCmd . route

---------------------------------------------------------------------
-- Generating help and information strings for display

runHelp :: [String] -> String
runHelp []            = H.displayHelp hub
runHelp ("run":_)     = H.directiveHelp "run"
runHelp ("help":_)    = H.directiveHelp "help"
runHelp ("version":_) = H.directiveHelp "version"
runHelp ("all":_)     = H.keywordHelp "all"
runHelp ("and":_)     = H.keywordHelp "and"
runHelp (",":_)       = H.keywordHelp "and"
runHelp ("with":_)    = H.keywordHelp "with"
runHelp ("+":_)       = H.keywordHelp "with"
runHelp ("copying":_) = H.copyingStr
runHelp xs            = intercalate "\n"
                        . map ( H.longCmdHelpStr . route )
                        $ xs

-- =============================================================== --
-- Finalization

finish :: Either String T.BtxState -> IO ()
-- ^Cleanup after running a script.
finish (Left msg) = putStr msg
finish (Right bs)
    | Tx.null lg = pure ()
    | otherwise  = Tx.putStrLn lg
    where lg = T.logger bs
