{-# LANGUAGE OverloadedStrings #-}

module Commands
    ( route
    , runHelp
    , updateIn
    , save
    ) where

import qualified Data.Text.IO           as Tx
import qualified Data.Text              as Tx
import qualified Data.Map.Strict        as Map
import qualified Types                  as T
import Data.Text                                ( Text              )
import Data.List                                ( foldl'
                                                , intercalate       )
import Data.Maybe                               ( mapMaybe          )
import Control.Monad                            ( unless            )
import Control.Monad.Except                     ( throwError        )
import Control.Monad.State.Lazy                 ( get
                                                , put
                                                , lift
                                                , liftIO            )
import CoreIO                                   ( readOrMakeFile
                                                , readFileExcept
                                                , writeFileExcept   )
import BibTeX.Parser                            ( parseBibliography )
import Formatting                               ( refToBibtex
                                                , formatRef
                                                , bibToBibtex
                                                , summarize
                                                , unrecognized
                                                , cannotRename      )

-- =============================================================== --
-- Hub and router

route :: String -> T.Command [T.Ref]
route c = go hub
    where go []     = T.Command "err" (errCmd c) [] []
          go (x:xs) | T.cmdName x == c = x
                    | otherwise        = go xs

hub :: [ T.Command [T.Ref] ]
hub = [ -- Bibliography managers
        T.Command "in"   inCmd   inCmdSHelp   inCmdLHelp
      , T.Command "to"   toCmd   toCmdSHelp   toCmdLHelp
      , T.Command "from" fromCmd fromCmdSHelp fromCmdLHelp
        -- Queries
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

-- =============================================================== --
-- Utilities

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

insert :: T.References -> [T.Ref] -> T.References
-- ^Update a reference map with a list of references.
insert = foldl' ( flip $ uncurry Map.insert )

delete :: T.References -> [T.Ref] -> T.References
delete refs = foldl' ( flip Map.delete ) refs . fst . unzip

save :: T.Bibliography -> T.BtxStateMonad ()
-- ^Convert a bibliography to BibTeX and write to memory.
save b = do
    liftIO . putStrLn $ "writing to " ++ T.path b
    lift . writeFileExcept (T.path b) . bibToBibtex $ b

getRef :: T.Bibliography -> String -> Maybe T.Ref
-- ^Lookup a reference from a bibliography and package as a Ref.
getRef bib x = (,) key <$> Map.lookup key ( T.refs bib )
    where key = Tx.pack x

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Commands
---------------------------------------------------------------------
---------------------------------------------------------------------

-- =============================================================== --
-- Bibliography constructors, operators and utilities

-- inCmd ------------------------------------------------------------

inCmdSHelp :: String
inCmdSHelp = "in FILE-PATH : reset or create new working bibliography"

inCmdLHelp :: String
inCmdLHelp = intercalate "\n" hs
    where hs = [ inCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Update working bibliography with the current context."
               , "  2. Save the updated working bibliography to disk."
               , "  3. Clear the current context."
               , "  4. Load the .bib file at FILE-PATH as the new working"
               , "     bibliography. If the file does not already exist, then"
               , "     it is created."
               ]

inCmd :: T.CommandMonad [T.Ref]
inCmd xs rs
    | null xs       = throwError "Command <in> requires a file path."
    | length xs > 1 = throwError "Command <in> allows only one argument."
    | otherwise     = do updateIn rs >>= save
                         btxState <- get
                         let fp = head xs
                         content <- lift . readOrMakeFile $ fp
                         case parseBibliography fp content of
                              Left e  -> throwError e
                              Right b -> do put btxState { T.inBib = b }
                                            return []

-- toCmd ------------------------------------------------------------

toCmdSHelp :: String
toCmdSHelp = "to FILE-PATH : reset or create new export bibliography"

toCmdLHelp :: String
toCmdLHelp = intercalate "\n" hs
    where hs = [ inCmdSHelp ++ "\n"
               , "The export bibliography is separate from the working"
               , "bibliography (set with <in> )and represents a target where"
               , "references can be exported using the <send> command. This"
               , "command has the following effects:\n"
               , "  1. Leave the current context unchanged."
               , "  2. Save the current export bibliography if it exists."
               , "  3. Load the .bib file at FILE-PATH as the new export"
               , "     bibliography. If the file does not exist, then it is"
               , "     created. If the file path is the same as that for the"
               , "     working bibliography, then the export bibliography"
               , "     is unset and nothing else happens.\n"
               , "See also help for the <send> command."
               ]

toCmd :: T.CommandMonad [T.Ref]
toCmd xs rs
    | null xs       = throwError "Command <to> requires a file path."
    | length xs > 1 = throwError "Command <to> allows only one argument."
    | otherwise     = do
        btxState <- get
        maybe ( return () ) save $ T.toBib btxState
        let fp = head xs
        if fp == ( T.path . T.inBib ) btxState
           then put btxState { T.toBib = Nothing }
           else do content <- lift . readOrMakeFile $ fp
                   case parseBibliography fp content of
                        Left e  -> throwError e
                        Right b -> put btxState { T.toBib = Just b }
        return rs

-- fromCmd ----------------------------------------------------------

fromCmdSHelp :: String
fromCmdSHelp = "from FILE-PATH : reset the import bibliography."

fromCmdLHelp :: String
fromCmdLHelp = intercalate "\n" hs
    where hs = [ fromCmdSHelp ++ "\n"
               , "The import bibliography is a bibliography separate from the"
               , "working bibliography (set with <in>) that you can use to"
               , "populate the context with references using the <take>"
               , "command. This is useful for building new bibliographies from"
               , "previously existing ones. The import bibliography is never"
               , "modified. The <from> command has the following effects:\n"
               , "  1. If FILE-PATH does not exist, then an error is generated."
               , "  2. If FILE-PATH is the same as the working bibliography,"
               , "     then the import bibliography becomes unset."
               , "  3. No matter what, the context is left unchanged.\n"
               , "See also help for the <take> command."
               ]

fromCmd :: T.CommandMonad [T.Ref]
-- ^Set the from-bibliography leaving the context unchanged. From-
-- bibliographies are never written.
fromCmd xs rs
    | null xs       = throwError "Commad <from> requires a file path."
    | length xs > 1 = throwError "Command <from> allows only one argument."
    | otherwise     =
        do btxState <- get
           let fp = head xs
           if fp == ( T.path . T.inBib ) btxState
              then put btxState { T.fromBib = Nothing }
              else do content <- lift . readFileExcept $ fp
                      case parseBibliography fp content of
                           Left e  -> throwError e
                           Right b -> put btxState { T.fromBib = Just b }
           return rs

-- =============================================================== --
-- Context constructors

---------------------------------------------------------------------

getCmd :: T.CommandMonad [T.Ref]
-- ^Update the in-bibliography with the current context then bring
-- new references into the context without changing the bibliography.
getCmd xs rs = do
    updateIn rs
    bib <- T.inBib <$> get
    return . mapMaybe ( getRef bib ) $ xs

---------------------------------------------------------------------

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

---------------------------------------------------------------------

takeCmd :: T.CommandMonad [T.Ref]
-- ^Extract a reference from the from-bibliography. If the
-- from-bibliography is not set, then default to the in-bibliography.
takeCmd xs _ = do
    btxState <- get
    case T.fromBib btxState of
         Nothing  -> getCmd xs []
         Just bib -> return . mapMaybe ( getRef bib ) $ xs

-- =============================================================== --
-- Context operators

---------------------------------------------------------------------

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

---------------------------------------------------------------------

tossCmd :: T.CommandMonad [T.Ref]
tossCmd _ rs = return []

---------------------------------------------------------------------

viewCmd :: T.CommandMonad [T.Ref]
viewCmd _ [] = do
    liftIO . putStrLn $ "\nNo entries to view.\n"
    return []
viewCmd _ rs = do
    liftIO . Tx.putStrLn $ Tx.empty
    liftIO . Tx.putStrLn . Tx.intercalate "\n\n" . map formatRef $ rs
    return rs

---------------------------------------------------------------------

nameCmd :: T.CommandMonad [T.Ref]
nameCmd ns rs
    | nn == nr  = return . zipWith ( \ n (k,v) -> (Tx.pack n, v) ) ns $ rs
    | otherwise = ( liftIO . putStrLn $ cannotRename nn nr ) >> return rs
    where nn = length ns
          nr = length rs

-- =============================================================== --
-- Queries

---------------------------------------------------------------------

infoCmd :: T.CommandMonad [T.Ref]
infoCmd _ rs = do
    bib <- T.inBib <$> get
    liftIO . Tx.putStrLn . summarize $ bib
    return rs

---------------------------------------------------------------------

listCmd :: T.CommandMonad [T.Ref]
listCmd _ rs = do
    bib <- T.inBib <$> get
    liftIO . mapM_ Tx.putStrLn . fst . unzip . Map.toList . T.refs $ bib
    return rs

-- =============================================================== --
-- Errors and help

---------------------------------------------------------------------

runHelp :: [String] -> String
-- ^Generate a help message
runHelp [] = intercalate "\n" . map T.cmdSHelp $ hub
runHelp xs = intercalate "\n" . map ( T.cmdLHelp . route ) $ xs

errCmd :: String -> T.CommandMonad [T.Ref]
errCmd c _ _ = throwError . unrecognized $ c
