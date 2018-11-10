{-# LANGUAGE OverloadedStrings #-}

module Core
    ( allKeysToArgs
    , deleteRefs
    , dropRefByKey
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

allKeysToArgs :: T.Bibliography -> [String]
-- ^Get a list of all keys in a bibliography represented as Strings.
-- This is useful for generating argument lists.
allKeysToArgs = map Tx.unpack . Map.keys . T.refs

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

dropRefByKey :: T.Context -> Tx.Text -> T.Context
-- ^Delete all entries in the context that have the indicated key.
dropRefByKey []                        _  = []
dropRefByKey (r@(T.Ref     _ k _ ):rs) k' | k == k'   = dropRefByKey rs k'
                                          | otherwise = r : dropRefByKey rs k'
dropRefByKey (r@(T.Missing _ k _ ):rs) k' | k == k'   = dropRefByKey rs k'
                                          | otherwise = r : dropRefByKey rs k'

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
-- ^Parses an input string into String-command-argument-list pairs.
parse = parseCmds . preprocess

parseCmds :: [String] -> T.Start T.ParsedCommand
-- ^First stage to parsing the user supplied list of commands. This
-- is where no-script components are intercepted.
parseCmds []            = T.Usage "This won't do anything (try: btx help)."
parseCmds ("in":xs)     = parseFirstIn . break ( == "and" ) $ xs
parseCmds ("help":xs)   = T.Help xs
parseCmds ("--help":xs) = T.Help xs
parseCmds ("-h":xs)     = T.Help xs
parseCmds xs            = T.Normal [] . parseAnd $ xs

parseFirstIn :: ([String], [String]) -> T.Start T.ParsedCommand
-- ^Parse the first in-command. This is necessary so we can know how
-- to load the initial working bibliography.
parseFirstIn ([],_)    = T.Usage "This won't do anything (try: btx help)."
parseFirstIn (_:_:_,_) = T.Usage "Command <in> allows only one argument."
parseFirstIn (x:_,cs)  = T.Normal x . parseAnd $ cs

preprocess :: String -> [String]
-- ^Convert all command separators to <and> keyword and remove
-- the <and with> keyword pairs to extend argument lists.
preprocess = handleWith . words . formatTokens
    where -- Reformat to use only <and> and <with>
          formatTokens []        = []
          formatTokens (',':xs)  = " and " ++ formatTokens xs
          formatTokens ('\n':xs) = " and " ++ formatTokens xs
          formatTokens ('+':xs)  = " with " ++ formatTokens xs
          formatTokens (x:xs)    = x : formatTokens xs
          -- Remove <and with> pairs
          handleWith []                = []
          handleWith ("and":"with":xs) = handleWith xs
          handleWith (x:xs)            = x : handleWith xs

parseAnd :: [String] -> [T.ParsedCommand]
-- ^Actually parse out the commands and arguments.
parseAnd []          = []
parseAnd ("and":xs)  = parseAnd xs
parseAnd (x:xs)      = (x, ys) : parseAnd zs
    where (ys,zs)  = break ( == "and" ) xs
