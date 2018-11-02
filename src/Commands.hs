{-# LANGUAGE OverloadedStrings #-}

module Commands
    ( compile
    , parseCmds
    , initBib
    , route
    ) where

import qualified Data.Text.IO           as Tx
import qualified Data.Text              as Tx
import qualified Data.Map.Strict        as Map
import qualified Types                  as T
import qualified Resources.Resources    as R
import Data.Text                                ( Text              )
import Data.List                                ( foldl'            )
import Data.Maybe                               ( mapMaybe          )
import Control.Monad                            ( zipWithM          )
import Control.Monad.Except                     ( throwError        )
import Control.Monad.State.Lazy                 ( get
                                                , put
                                                , lift
                                                , liftIO            )
import CoreIO                                   ( safeReadFile
                                                , safeWriteFile     )
import BibTeXParser                             ( parseBibtex       )
import Formatting                               ( refToBibtex
                                                , formatRef
                                                , bibToBibtex
                                                , summarize         )

-- =============================================================== --
-- Initialization

-- Exported

initBib :: T.Bibliography -> T.BtxState
initBib b =  T.BtxState { T.inBib   = b
                        , T.toBib   = Nothing
                        , T.fromBib = Nothing
                        }

-- =============================================================== --
-- Parsing and compiling

---------------------------------------------------------------------
-- Compiling

-- Exported

compile :: [ T.Command [T.Ref] ] -> [T.Ref] -> T.BtxStateMonad ()
compile []                          rs = finalize rs
compile ((T.Command _ m xs _) : cs) rs = m xs rs >>= compile cs

---------------------------------------------------------------------
-- Parsing

-- Exported

parseCmds :: String -> [ T.Command [T.Ref] ]
parseCmds = parseAnd . words . splitAnd

-- Unexported

splitAnd :: String -> String
splitAnd []        = []
splitAnd (',':xs)  = " and " ++ splitAnd xs
splitAnd ('\n':xs) = " and " ++ splitAnd xs
splitAnd (x:xs)    = x : splitAnd xs

parseAnd :: [String] -> [ T.Command [T.Ref] ]
parseAnd []          = []
parseAnd ("and":xs)  = parseAnd xs
parseAnd (x:xs)      = route x ys : parseAnd zs
    where (ys,zs) = break ( == "and" ) xs

-- =============================================================== --
-- Command router

-- Exported

route :: String -> [String] -> T.Command [T.Ref]
-- Bibliography managers
route "in"   xs = T.Command "in"   inCmd   xs "in help"
route "to"   xs = T.Command "to"   toCmd   xs "to help"
route "from" xs = T.Command "from" fromCmd xs "from help"
-- Queries
route "help" xs = T.Command "help" helpCmd xs "help help"
route "info" xs = T.Command "info" infoCmd xs "info help"
-- Context constructors
route "get"  xs = T.Command "get"  getCmd  xs "get help"
route "take" xs = T.Command "take" takeCmd xs "take help"
-- Context operators
route "name" xs = T.Command "name" nameCmd xs "name help"
route "send" xs = T.Command "send" sendCmd xs "send help"
route "view" xs = T.Command "view" viewCmd xs "view help"
-- Unrecognized commands
route c      _  = T.Command "err"  ( errCmd c ) [] ( "no commad " ++ c )

-- =============================================================== --
-- Commands

---------------------------------------------------------------------
-- Bibliography constructors, operators and utilities

-- Exposed to user

inCmd :: T.CommandMonad [T.Ref]
-- ^Set the in-bibliography. Add the current context to the previous
-- in-bibliography, save and clear the context before loading the new
-- in-bibliography.
inCmd xs rs
    | null xs       = throwError "Command <in> requires a file path."
    | length xs > 1 = throwError "Command <in> allows only one argument."
    | otherwise     = do updateIn rs >>= save
                         btxState <- get
                         let fp = head xs
                         content <- lift . safeReadFile $ fp
                         case parseBibtex fp content of
                              Left e  -> throwError e
                              Right b -> do put btxState { T.inBib = b }
                                            return []

toCmd :: T.CommandMonad [T.Ref]
-- ^Set the to-bibliography leaving the context unchanged. Write the
-- current to-bibliograpy without updating with the current context.
toCmd xs rs
    | null xs       = throwError "Command <to> requires a file path."
    | length xs > 1 = throwError "Command <to> allows only one argument."
    | otherwise     = do btxState <- get
                         maybe ( return () ) save $ T.toBib btxState
                         let fp = head xs
                         content <- lift . safeReadFile $ fp
                         case parseBibtex fp content of
                              Left e  -> throwError e
                              Right b -> put btxState { T.toBib = Just b }
                         return rs

fromCmd :: T.CommandMonad [T.Ref]
-- ^Set the from-bibliography leaving the context unchanged. From-
-- bibliographies are never written.
fromCmd xs rs
    | null xs       = throwError "Commad <from> requires a file path."
    | length xs > 1 = throwError "Command <from> allows only one argument."
    | otherwise     = do btxState <- get
                         let fp = head xs
                         content <- lift . safeReadFile $ fp
                         case parseBibtex fp content of
                              Left e  -> throwError e
                              Right b -> put btxState { T.fromBib = Just b }
                         return rs

-- Hidden

updateIn :: [T.Ref] -> T.BtxStateMonad T.Bibliography
-- ^Save references in context to the in-bibliography and return the
-- updated bibliography.
updateIn rs = do
    btxState <- get
    let oldBib  = T.inBib btxState
        newRefs = insert ( T.refs oldBib ) rs
        newBib  = oldBib { T.refs = newRefs }
    put btxState { T.inBib = newBib }
    return newBib

finalize :: [T.Ref] -> T.BtxStateMonad ()
-- ^Save the current context references to the in-bibliography and
-- write both the in- and to-bibliographies.
finalize rs = do
    updateIn rs >>= save
    btxState <- get
    maybe ( return () ) save . T.toBib $ btxState

insert :: T.References -> [T.Ref] -> T.References
-- ^Update a reference map with a list of references.
insert = foldl' ( flip $ uncurry Map.insert )

save :: T.Bibliography -> T.BtxStateMonad ()
-- ^Convert a bibliography to BibTeX and write to memory.
save b = do
    liftIO . putStrLn $ "writing to " ++ T.path b
    lift . safeWriteFile (T.path b) . bibToBibtex $ b

errCmd :: String -> T.CommandMonad [T.Ref]
errCmd c _ _ = throwError . R.unrecognized $ c

---------------------------------------------------------------------
-- Context constructors

-- Exposed to user

getCmd :: T.CommandMonad [T.Ref]
-- ^Extract a reference from the in-bibliography.
getCmd xs _ = do
    bib <- T.inBib <$> get
    return . mapMaybe ( getRef bib ) $ xs

takeCmd :: T.CommandMonad [T.Ref]
-- ^Extract a reference from the from-bibliography. If the
-- from-bibliography is not set, then default to the in-bibliography.
takeCmd xs _ = do
    btxState <- get
    case T.toBib btxState of
         Nothing -> getCmd xs []
         Just b  -> return . mapMaybe ( getRef b ) $ xs

-- Hidden

getRef :: T.Bibliography -> String -> Maybe T.Ref
getRef bib x = (,) key <$> Map.lookup key ( T.refs bib )
    where key = Tx.pack x

---------------------------------------------------------------------
-- Context operators

-- Exposed user

sendCmd :: T.CommandMonad [T.Ref]
-- ^Save references in context to the to-bibliography and depopulate
-- the context. Treat the in-bibliography as the to-bibliography if
-- the to-bibliography is Nothing.
sendCmd _ rs = do
    btxState <- get
    case T.toBib btxState of
         Nothing     -> updateIn rs >> return []
         Just oldBib -> do let newRefs = insert ( T.refs oldBib ) rs
                               newBib  = oldBib { T.refs = newRefs }
                           put btxState { T.toBib = Just newBib }
                           return []

viewCmd :: T.CommandMonad [T.Ref]
viewCmd _ [] = do
    liftIO . putStrLn $ "\nNo entries to view.\n"
    return []
viewCmd _ rs = do
    liftIO . Tx.putStrLn $ Tx.empty
    liftIO . Tx.putStrLn . Tx.intercalate "\n\n" . map formatRef $ rs
    return rs

nameCmd :: T.CommandMonad [T.Ref]
nameCmd ns rs
    | nn == nr = zipWithM rename ns rs
    | otherwise = ( liftIO . putStrLn $ R.cannotRename nn nr ) >> return rs
    where nn = length ns
          nr = length rs

-- Hidden

rename :: String -> T.Ref -> T.BtxStateMonad T.Ref
rename n (k,v) = do
    bst <- get
    let newKey  = Tx.pack n
        oldBib  = T.inBib bst
        newRefs = Map.insert newKey v . Map.delete k . T.refs $ oldBib
    put bst { T.inBib = oldBib { T.refs = newRefs } }
    return (newKey, v)

---------------------------------------------------------------------
-- Queries

helpCmd :: T.CommandMonad [T.Ref]
-- ^Display help leaving the context unchanged.
helpCmd [] rs = helpCmd ["help"] rs
helpCmd xs rs = do
    liftIO . mapM_ ( putStrLn . T.cmdHelp . flip route [] ) $ xs
    return rs

infoCmd :: T.CommandMonad [T.Ref]
infoCmd _ _ = do
    bib <- T.inBib <$> get
    liftIO . Tx.putStrLn . summarize $ bib
    return []
