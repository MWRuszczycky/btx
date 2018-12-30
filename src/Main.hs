{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text      as Tx
import qualified Types          as T
import System.Environment               ( getArgs             )
import System.Directory                 ( listDirectory
                                        , getCurrentDirectory )
import Control.Monad.State.Lazy         ( execStateT, get
                                        , liftIO              )
import Control.Monad.Except             ( runExceptT
                                        , throwError
                                        , liftEither          )
import BibTeX.Parser                    ( parseBib            )
import Core                             ( parse               )
import CoreIO                           ( readOrMakeFile      )
import Formatting                       ( uniqueBibErr        )
import Commands                         ( route, saveCmd
                                        , runHelp             )

-- =============================================================== --
-- Entry point and clean up

main :: IO ()
main = do
    script <- getScript =<< getArgs
    case parse script of
         T.Usage msg    -> finish . Left $ msg
         T.Help cs      -> finish . Left . runHelp $ cs
         T.Normal fp cs -> runExceptT ( initBtx cs fp >>= runBtx )
                           >>= finish

finish :: Either String T.BtxState -> IO ()
finish (Left msg) = putStr msg
finish (Right _ ) = return ()

-- =============================================================== --
-- Initialization

getScript :: [String] -> IO String
-- ^Find the script that the user wants to run.
getScript ("run":fp:_) = readFile fp            -- Script is in a file.
getScript ("run":[])   = getContents            -- Script is from stdin.
getScript xs           = return . unwords $ xs  -- Script is from command line.

initBtx :: a -> FilePath -> T.ErrMonad ( a, T.BtxState )
-- ^Generate the initial state.
initBtx x [] = initDefaultBtxState x
initBtx x fp = do
    content <- readOrMakeFile fp
    bib     <- liftEither . parseBib fp $ content
    return ( x, T.BtxState bib Nothing Nothing Tx.empty )

initDefaultBtxState :: a -> T.ErrMonad (a, T.BtxState)
-- ^User has not specified a working bibliography, so look in current
-- working directory for a unique .bib file to use instead.
initDefaultBtxState x = do
    cwd <- liftIO getCurrentDirectory
    fps <- liftIO . listDirectory $ cwd
    case filter ( (== "bib.") . take 4 . reverse ) fps of
         (fp:[])   -> initBtx x fp
         otherwise -> throwError . uniqueBibErr $ cwd

-- =============================================================== --
-- Compiling and running

runBtx :: ( [T.ParsedCommand], T.BtxState ) -> T.ErrMonad T.BtxState
-- ^Compile and run the commands an the initial state.
runBtx (cs, st) = execStateT ( compile cs [] ) st

compile :: [T.ParsedCommand] -> T.Context -> T.BtxStateMonad T.Context
-- ^Compile parsed commands (i.e., String-String list pairs) into a
-- runnable BtxStateMonad.
compile []                 rs = saveCmd [] rs
compile ( (c, args) : cs ) rs = applyCmd c args rs >>= compile cs
    where applyCmd = T.cmdCmd . route
