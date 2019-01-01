{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Core
    ( allKeysToArgs
    , deleteRefs
    , dropRefByKey
    , insertRefs
    , isPresent
    , getRef
    , pairToRef
    , refToPair
    ) where

-- =============================================================== --
-- DSL for working with modeled bibliography information
-- =============================================================== --

import qualified Data.Map.Strict  as Map
import qualified Data.Text        as Tx
import qualified Model.Core.Types as T
import Data.Text                         ( Text     )
import Data.List                         ( foldl'   )
import Data.Maybe                        ( mapMaybe )

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
