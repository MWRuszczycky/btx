{-# LANGUAGE OverloadedStrings #-}

module Controller.Controller
    ( configureBtx
    , runBtx
    ) where

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
                                                , getCurrentDirectory
                                                , getHomeDirectory
                                                , doesFileExist       )
import           System.IO                      ( stdout
                                                , hIsTerminalDevice   )
import           Control.Monad.State.Lazy       ( execStateT
                                                , foldM, liftIO       )
import           Control.Monad.Except           ( throwError
                                                , liftEither          )

-- =============================================================== --
-- Configuration

configureBtx :: [String] -> T.ErrMonad T.Config
configureBtx args = configInit >>= configArgs args >>= configFinal

configInit :: T.ErrMonad T.Config
configInit = do
    path   <- liftIO $ fmap (<> "/.config/btx/config") getHomeDirectory
    exists <- liftIO . doesFileExist $ path
    if not exists
       then pure T.defaultConfig
       else E.readFileExcept path
            >>= liftEither . PC.parseConfig
            >>= foldM (flip ($)) T.defaultConfig

configArgs :: [String] -> T.Config -> T.ErrMonad T.Config
configArgs args config = liftEither fs >>= foldM (flip ($)) config
    where fs = PC.parseInput . PC.formatInput $ args

configFinal :: T.Config -> T.ErrMonad T.Config
configFinal config = do
    isTerm <- liftIO . hIsTerminalDevice $ stdout
    if isTerm && T.cUseANSI config
       then pure $ config { T.cStyles = V.defaultStyles }
       else pure $ config { T.cStyles = V.noStyles      }

-- =============================================================== --
-- Running btx after configuration

runBtx :: T.Config -> T.ErrMonad Text
runBtx config =
    case T.cDirective config of
         T.Help xs       -> pure $ H.getHelp (T.cStyles config) C.hub xs
         T.Version       -> pure $ H.getHelp (T.cStyles config) C.hub ["version"]
         T.RunFile fp    -> E.readFileExcept fp >>= runScript config
         T.RunStdIn      -> liftIO getContents  >>= runScript config . Tx.pack
         T.Script script -> runScript config script

runScript :: T.Config -> Text -> T.ErrMonad Text
runScript config script = do
    (path, cmds) <- liftEither . PC.parseScript $ script
    inBib        <- getInBib path
    let btx = T.BtxState inBib Nothing Nothing Tx.empty config
    runCommands btx cmds >>= pure . T.logger

runCommands :: T.BtxState -> [T.ParsedCommand] -> T.ErrMonad T.BtxState
runCommands btx cmds = execStateT ( foldM runCmd [] cmds ) btx
    where runCmd = flip . uncurry $ T.cmdCmd . C.route

---------------------------------------------------------------------
-- Finding the initial working BibTeX bibliography

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
