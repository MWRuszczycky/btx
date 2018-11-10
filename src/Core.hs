{-# LANGUAGE OverloadedStrings #-}

module Core
    ( deleteRefs
    , insertRefs
    , isPresent
    , getRef
    , pairToRef
    , parse
    , refToPair
    , updateIn
    , updateTo
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Tx
import qualified Types           as T
import Data.Text                        ( Text              )
import Data.List                        ( foldl'            )
import Data.Maybe                       ( mapMaybe          )
import Control.Monad.State.Lazy         ( get, put          )


-- =============================================================== --
-- Utilities for managing references, bibliographies and state

refToPair :: T.Ref -> Maybe (Text, T.Entry)
refToPair (T.Ref _ k v     ) = Just (k, v)
refToPair (T.Missing _ _ _ ) = Nothing

pairToRef :: FilePath -> (Text, T.Entry) -> T.Ref
pairToRef fp (k, v) = T.Ref fp k v

isPresent :: T.Ref -> Bool
isPresent (T.Ref _ _ _     ) = True
isPresent (T.Missing _ _ _ ) = False

insertRefs :: T.References -> T.Context -> T.References
-- ^Update a reference map with a list of references.
insertRefs refs = foldl' go refs . mapMaybe refToPair
    where go = flip $ uncurry Map.insert

deleteRefs :: T.References -> T.Context -> T.References
-- ^Update a reference map by deleting references in a list.
deleteRefs refs = foldl' go refs . fst . unzip . mapMaybe refToPair
    where go = flip Map.delete

getRef :: T.Bibliography -> String -> T.Ref
-- ^Lookup a reference from a bibliography and package as a Ref.
getRef bib x = let key = Tx.pack x
               in  case Map.lookup key ( T.refs bib ) of
                        Nothing -> T.Missing ( T.path bib ) key "no such entry"
                        Just v  -> T.Ref ( T.path bib ) key v

updateIn :: T.Context -> T.BtxStateMonad T.Bibliography
-- ^Save references in context to the in-bibliography and return the
-- updated bibliography.
updateIn rs = do
    btxState <- get
    let oldBib  = T.inBib btxState
        newRefs = insertRefs ( T.refs oldBib ) rs
        newBib  = oldBib { T.refs = newRefs }
    put btxState { T.inBib = newBib }
    return newBib

updateTo :: T.Context -> T.BtxStateMonad ( Maybe T.Bibliography )
-- ^Save references in context to the to-bibliography and return the
-- updated bibligraphy.
updateTo rs = do
    btxState <- get
    let newBib = do oldBib <- T.toBib btxState    -- Maybe monad here
                    return oldBib { T.refs = insertRefs ( T.refs oldBib ) rs }
    put btxState { T.toBib = newBib }
    return newBib

-- =============================================================== --
-- Parsing

parse :: String -> T.Start T.ParsedCommand
parse = parseCmds . words . splitAnd

parseCmds :: [String] -> T.Start T.ParsedCommand
parseCmds []            = T.Usage "This won't do anything (try: btx help)."
parseCmds ("in":xs)     = parseFirstIn . break ( == "and" ) $ xs
parseCmds ("help":xs)   = T.Help xs
parseCmds ("--help":xs) = T.Help xs
parseCmds ("-h":xs)     = T.Help xs
parseCmds xs            = T.Normal [] . parseAnd $ xs

parseFirstIn :: ([String], [String]) -> T.Start T.ParsedCommand
parseFirstIn ([],_)    = T.Usage "This won't do anything (try: btx help)."
parseFirstIn (_:_:_,_) = T.Usage "Command <in> allows only one argument."
parseFirstIn (x:_,cs)  = T.Normal x . parseAnd $ cs

splitAnd :: String -> String
splitAnd []        = []
splitAnd (',':xs)  = " and " ++ splitAnd xs
splitAnd ('\n':xs) = " and " ++ splitAnd xs
splitAnd (x:xs)    = x : splitAnd xs

parseAnd :: [String] -> [T.ParsedCommand]
parseAnd []          = []
parseAnd ("and":xs)  = parseAnd xs
parseAnd (x:xs)      = (x, ys) : parseAnd zs
    where (ys,zs)  = break ( == "and" ) xs
