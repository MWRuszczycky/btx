{-# LANGUAGE OverloadedStrings #-}

module Commands
    ( compile
    , parseCmds
    , initBtxState
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
import Control.Monad.Except                     ( throwError        )
import Control.Monad.State.Lazy                 ( get
                                                , put
                                                , lift
                                                , liftIO            )
import CoreIO                                   ( safeReadFile
                                                , safeWriteFile     )
import BibTeXParser                             ( parseBibliography )
import Formatting                               ( refToBibtex
                                                , formatRef
                                                , bibToBibtex
                                                , summarize         )

-- =============================================================== --
-- Initialization

-- Exported

initBtxState :: T.Bibliography -> T.BtxState
initBtxState b =  T.BtxState { T.inBib   = b
                             , T.toBib   = Nothing
                             , T.fromBib = Nothing
                             }

-- =============================================================== --
-- Parsing and compiling

---------------------------------------------------------------------
-- Compiling

-- Exported

compile :: [ T.CommandArgsMonad [T.Ref] ] -> [T.Ref] -> T.BtxStateMonad ()
compile []        rs = finalize rs
compile ( c : cs) rs = c rs >>= compile cs

---------------------------------------------------------------------
-- Parsing

-- Exported

parseCmds :: String -> ( [ T.CommandArgsMonad [T.Ref] ], FilePath )
parseCmds xs = case words . splitAnd $ xs of
                    []           -> ( [],          ""                   )
                    ("in":fp:cs) -> ( parseAnd cs, fp                   )
                    cs           -> ( parseAnd cs, "test/files/new.bib" )

-- Unexported

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

-- =============================================================== --
-- Command router

-- Exported

hub :: [ T.Command [T.Ref] ]
hub = [ -- Bibliography managers
        T.Command "in"   inCmd   "short help" "in help"
      , T.Command "to"   toCmd   "short help" "to help"
      , T.Command "from" fromCmd "short help" "from help"
        -- Queries
      , T.Command "help" helpCmd "short help" "help help"
      , T.Command "info" infoCmd "short help" "info help"
      , T.Command "list" listCmd "short help" "list help"
        -- Context constructors
      , T.Command "get"  getCmd  "short help" "get help"
      , T.Command "pull" pullCmd "short help" "pull help"
      , T.Command "take" takeCmd "short help" "take help"
        -- Context operators
      , T.Command "name" nameCmd "short help" "name help"
      , T.Command "send" sendCmd "short help" "send help"
      , T.Command "toss" tossCmd "short help" "toss help"
      , T.Command "view" viewCmd "short help" "view help"
      ]

route :: String -> T.Command [T.Ref]
route c = go hub
    where go []     = T.Command "err" (errCmd c) "" ("no command " <> Tx.pack c)
          go (x:xs) | T.cmdName x == c = x
                    | otherwise        = go xs

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
                         case parseBibliography fp content of
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
                         case parseBibliography fp content of
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
                         case parseBibliography fp content of
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

delete :: T.References -> [T.Ref] -> T.References
delete refs = foldl' ( flip Map.delete ) refs . fst . unzip

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
-- ^Update the in-bibliography with the current context then bring
-- new references into the context without changing the bibliography.
getCmd xs rs = do
    updateIn rs
    bib <- T.inBib <$> get
    return . mapMaybe ( getRef bib ) $ xs

pullCmd :: T.CommandMonad [T.Ref]
-- ^Update the in-bibliography with the current context then bring
-- new references into the context deleting them from the
-- in-bibliography.
pullCmd xs rs = do
    updateIn rs
    btxState <- get
    let bib = T.inBib btxState
        rs' = mapMaybe ( getRef bib ) xs
    put btxState { T.inBib = bib { T.refs = delete ( T.refs bib ) rs' } }
    return rs'

takeCmd :: T.CommandMonad [T.Ref]
-- ^Extract a reference from the from-bibliography. If the
-- from-bibliography is not set, then default to the in-bibliography.
takeCmd xs _ = do
    btxState <- get
    case T.toBib btxState of
         Nothing  -> getCmd xs []
         Just bib -> return . mapMaybe ( getRef bib ) $ xs

-- Hidden

getRef :: T.Bibliography -> String -> Maybe T.Ref
getRef bib x = (,) key <$> Map.lookup key ( T.refs bib )
    where key = Tx.pack x

---------------------------------------------------------------------
-- Context operators

-- Exposed user

sendCmd :: T.CommandMonad [T.Ref]
-- ^Update the to-bibliography with the context and depopulate it.
-- If there is no to-bibliography, then just depopulate the context.
sendCmd ("to":xs) rs = toCmd xs rs >>= sendCmd []
sendCmd _ rs = do
    btxState <- get
    case T.toBib btxState of
         Nothing     -> return []
         Just oldBib -> do let newRefs = insert ( T.refs oldBib ) rs
                               newBib  = oldBib { T.refs = newRefs }
                           put btxState { T.toBib = Just newBib }
                           return []

tossCmd :: T.CommandMonad [T.Ref]
tossCmd _ rs = return []

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
    | nn == nr  = return . zipWith ( \ n (k,v) -> (Tx.pack n, v) ) ns $ rs
    | otherwise = ( liftIO . putStrLn $ R.cannotRename nn nr ) >> return rs
    where nn = length ns
          nr = length rs

---------------------------------------------------------------------
-- Queries

helpCmd :: T.CommandMonad [T.Ref]
-- ^Display help leaving the context unchanged.
helpCmd [] rs = helpCmd ["help"] rs
helpCmd xs rs = do
    liftIO . mapM_ ( Tx.putStrLn . T.cmdLHelp . route ) $ xs
    return rs

infoCmd :: T.CommandMonad [T.Ref]
infoCmd _ rs = do
    bib <- T.inBib <$> get
    liftIO . Tx.putStrLn . summarize $ bib
    return rs

listCmd :: T.CommandMonad [T.Ref]
listCmd _ rs = do
    bib <- T.inBib <$> get
    liftIO . mapM_ Tx.putStrLn . fst . unzip . Map.toList . T.refs $ bib
    return rs
