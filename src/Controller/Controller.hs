{-# LANGUAGE OverloadedStrings #-}

module Controller.Controller
    ( finish
    , getInput
    , getStyleMap
    , initBtx
    , getHelp
    , runBtx
    ) where

-- =============================================================== --
-- Provides the interface between the user and the model
-- =============================================================== --

import qualified View.Help                as H
import qualified Model.Types              as T
import qualified Data.Text                as Tx
import qualified Data.Text.IO             as Tx
import           Data.Text                      ( Text, pack          )
import           System.Directory               ( listDirectory
                                                , getCurrentDirectory )
import           System.IO                      ( stdout
                                                , getContents
                                                , hIsTerminalDevice   )
import           Control.Monad.State.Lazy       ( execStateT
                                                , foldM, liftIO       )
import           Control.Monad.Except           ( throwError
                                                , liftEither          )
import           Model.Parsers.BibTex           ( parseBib            )
import           Controller.ErrMonad            ( readOrMakeFile      )
import           View.View                      ( noStyles
                                                , defaultStyles       )
import           Controller.Commands            ( hub, route,         )

-- =============================================================== --
-- Initialization

---------------------------------------------------------------------
-- Finding the script or directive that the user wants to run

getInput :: [String] -> IO (Either String Text)
getInput ("run":fp:_) = Right <$> Tx.readFile fp
getInput ("run":[])   = pure . pack <$> getContents
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

helpCommands :: [T.HelpInfo]
helpCommands = map T.cmdHelp hub

getHelp :: T.StyleMap -> [String] -> Text
getHelp sm []            = H.mainHelp sm helpCommands
getHelp _  ("copying":_) = H.copyingStr
getHelp sm xs            = Tx.intercalate "\n" . map (H.showHelp sm hs) $ xs
    where hs = helpCommands ++ H.keywords ++ H.directives

-- =============================================================== --
-- Finalization

finish :: Either String T.BtxState -> IO ()
-- ^Cleanup after running a script.
finish (Left msg) = putStr msg
finish (Right bs)
    | Tx.null lg = pure ()
    | otherwise  = Tx.putStrLn lg
    where lg = T.logger bs
