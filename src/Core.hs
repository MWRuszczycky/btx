{-# LANGUAGE OverloadedStrings #-}

module Core
    ( initBtx
    , runBtx
    , parseCmds
    ) where

import qualified Types          as T
import Control.Monad.State.Lazy         ( execStateT, get   )
import Control.Monad.Except             ( throwError
                                        , liftEither        )
import CoreIO                           ( safeReadFile      )
import BibTeX.Parser                    ( parseBibliography )
import Commands                         ( route, save
                                        , updateIn          )


-- =============================================================== --
-- Initialization

-- Exported

initBtx :: ( a, FilePath ) -> T.ErrMonad ( a, T.BtxState )
-- ^Generate the initial state.
initBtx ( _, [] ) = throwError "No .bib file specified."
initBtx ( x, fp ) = do
    content <- safeReadFile fp
    bib <- liftEither . parseBibliography fp $ content
    return ( x, initBtxState bib )

initBtxState :: T.Bibliography -> T.BtxState
initBtxState b =  T.BtxState { T.inBib   = b
                             , T.toBib   = Nothing
                             , T.fromBib = Nothing
                             }

-- =============================================================== --
-- Compiling and running

runBtx :: ([T.CommandArgsMonad [T.Ref]], T.BtxState) -> T.ErrMonad T.BtxState
-- ^Compile and run the commands an the initial state.
runBtx (cmds, st) = execStateT ( compile cmds [] ) st

---------------------------------------------------------------------
-- Compiling

compile :: [ T.CommandArgsMonad [T.Ref] ] -> [T.Ref] -> T.BtxStateMonad ()
compile []        rs = finalize rs
compile ( c : cs) rs = c rs >>= compile cs

finalize :: [T.Ref] -> T.BtxStateMonad ()
-- ^Save the current context references to the in-bibliography and
-- write both the in- and to-bibliographies.
finalize rs = do
    updateIn rs >>= save
    btxState <- get
    maybe ( return () ) save . T.toBib $ btxState

---------------------------------------------------------------------
-- Parsing

parseCmds :: String -> ( [ T.CommandArgsMonad [T.Ref] ], FilePath )
parseCmds xs = case words . splitAnd $ xs of
                    []           -> ( [],          ""                   )
                    ("in":fp:cs) -> ( parseAnd cs, fp                   )
                    cs           -> ( parseAnd cs, "test/files/new.bib" )

splitAnd :: String -> String
splitAnd []        = []
splitAnd (',':xs)  = " and " ++ splitAnd xs
splitAnd ('\n':xs) = " and " ++ splitAnd xs
splitAnd (x:xs)    = x : splitAnd xs

parseAnd :: [String] -> [ T.CommandArgsMonad [T.Ref] ]
parseAnd []          = []
parseAnd ("and":xs)  = parseAnd xs
parseAnd (x:xs)      = applyCmd x ys : parseAnd zs
    where (ys,zs)  = break ( == "and" ) xs
          applyCmd = T.cmdCmd . route
