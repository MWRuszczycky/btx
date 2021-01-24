{-# LANGUAGE OverloadedStrings #-}

module Controller.Controller
    ( configureBtx
    , runBtx
    ) where

-- =============================================================== --
-- Provides the interface between the user and the model
-- =============================================================== --

import qualified Controller.Commands      as C
import qualified Model.Core.ErrMonad      as E
import qualified View.Help                as H
import qualified Model.Parsers.BibTex     as PB
import qualified Model.Parsers.Config     as PC
import qualified Model.Core.Types         as T
import qualified Data.Text                as Tx
import qualified View.View                as V
import           Data.Text                      ( Text                )
import           System.Directory               ( listDirectory
                                                , getCurrentDirectory )
import           System.IO                      ( stdout
                                                , hIsTerminalDevice   )
import           Control.Monad.State.Lazy       ( execStateT
                                                , foldM, liftIO       )
import           Control.Monad.Except           ( throwError
                                                , liftEither          )

-- =============================================================== --
-- Configuration
-- TODO: This will need to be refactored eventually
--       The styles selection especially needs to be fixed.
--       Including changing the type. This will need to be done when
--       the View module is refactored.

configureBtx :: Text -> T.ErrMonad T.Config
configureBtx cmdLine = do
    opts   <- liftEither . PC.parseConfig $ cmdLine
    config <- foldM (flip ($)) T.defaultConfig opts
    styles <- getStyleMap
    pure $ config { T.cStyles = styles }

getStyleMap :: T.ErrMonad T.StyleMap
-- ^Determine which style map should be used. If stdout is a terminal
-- then a colored style map should be used. Otherwise, a plain style
-- map should be used so that escape sequences are not inserted.
getStyleMap = do
    useStyles <- liftIO $ hIsTerminalDevice stdout
    if useStyles
       then pure V.defaultStyles
       else pure V.noStyles

---------------------------------------------------------------------
-- Finding the working BibTeX bibliography that the user wants to use
-- and initializing the btx state with it

getInBib :: Maybe FilePath -> T.ErrMonad T.Bibliography
-- ^Find and parse the in bibliography
getInBib Nothing   = findUniqueBibFile   >>= getInBib . Just
getInBib (Just fp) = E.readOrMakeFile fp >>= liftEither . PB.parseBib fp

findUniqueBibFile :: T.ErrMonad FilePath
-- ^Look in current working directory for a unique .bib file.
findUniqueBibFile = do
    cwd <- liftIO getCurrentDirectory
    fps <- liftIO . listDirectory $ cwd
    case filter ( (== "bib.") . take 4 . reverse ) fps of
         (fp:[]) -> pure fp
         _       -> throwError . H.uniqueBibErr $ cwd

-- =============================================================== --
-- Running btx after configuration

runBtx :: T.Config -> T.ErrMonad Text
runBtx config
    | helpReq   = pure $ H.getHelp (T.cStyles config) C.hub (T.cHelp config)
    | otherwise = initBtx config >>= runCommands >>= finish
    where helpReq = not . null . T.cHelp $ config
          finish  = pure . T.logger

initBtx :: T.Config -> T.ErrMonad T.BtxState
initBtx config = do
    (path, cmds) <- liftEither . PC.parseScript . T.cScript $ config
    inBib        <- getInBib path
    pure $ T.BtxState inBib Nothing Nothing cmds Tx.empty (T.cStyles config)

runCommands :: T.BtxState -> T.ErrMonad T.BtxState
runCommands btx = execStateT ( foldM runCmd [] cmds ) btx
    where cmds   = T.commands btx
          runCmd = flip . uncurry $ T.cmdCmd . C.route
