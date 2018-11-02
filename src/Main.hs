{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Types          as T
import System.Environment               ( getArgs      )
import Control.Monad.State.Lazy         ( execStateT   )
import Control.Monad.Except             ( liftEither
                                        , throwError
                                        , runExceptT   )
import BibTeXParser                     ( parseBibtex  )
import Commands                         ( compile
                                        , initBib
                                        , parseCmds    )
import CoreIO                           ( safeReadFile )

---------------------------------------------------------------------
-- Main

main :: IO ()
main = do
    cmds   <- parseCmds . unwords <$> getArgs
    result <- runExceptT $ finalize =<< runBtx =<< initBtx cmds
    case result of
         Left msg  -> putStrLn msg
         otherwise -> putStrLn "\nDone."

initBtx :: [T.Command [T.Ref]] -> T.ErrMonad ([T.Command [T.Ref]], T.BtxState)
initBtx ( (T.Command "in" _ (x:[]) _ ) : cs ) = do
    bib <- liftEither . parseBibtex x =<< safeReadFile x
    return ( cs, initBib bib )
initBtx ( (T.Command "in" _ (x:xs) _ ) : cs ) =
    throwError "Only one argument allowed to <in>."
initBtx cs = do
    let fp = "test/files/new.bib"
    bib <- liftEither . parseBibtex fp =<< safeReadFile fp
    return ( cs, initBib bib )

runBtx :: ([ T.Command [T.Ref] ], T.BtxState) -> T.ErrMonad T.BtxState
runBtx (cmds, bst) = execStateT ( compile cmds [] ) bst

finalize :: T.BtxState -> T.ErrMonad ()
finalize _ = return ()
