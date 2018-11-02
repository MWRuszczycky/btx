{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Types          as T
import System.Environment               ( getArgs           )
import Control.Monad.State.Lazy         ( execStateT        )
import Control.Monad.Except             ( liftEither
                                        , throwError
                                        , runExceptT        )
import BibTeXParser                     ( parseBibliography )
import Commands                         ( compile
                                        , initBtxState
                                        , parseCmds         )
import CoreIO                           ( safeReadFile      )

---------------------------------------------------------------------
-- Main

main :: IO ()
-- ^Entry point.
main = do
    script <- parseCmds . unwords <$> getArgs
    result <- runExceptT $ runBtx =<< initBtx script
    case result of
         Left msg  -> putStrLn msg
         otherwise -> putStrLn "\nDone."

initBtx :: ( a, FilePath ) -> T.ErrMonad ( a, T.BtxState )
-- ^Generate the initial state.
initBtx ( _, [] ) = throwError "No .bib file specified."
initBtx ( x, fp ) = do
    content <- safeReadFile fp
    bib <- liftEither . parseBibliography fp $ content
    return ( x, initBtxState bib )

runBtx :: ([T.CommandArgsMonad [T.Ref]], T.BtxState) -> T.ErrMonad T.BtxState
-- ^Compile and run the commands an the initial state.
runBtx (cmds, st) = execStateT ( compile cmds [] ) st
