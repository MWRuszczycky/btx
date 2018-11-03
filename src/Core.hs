{-# LANGUAGE OverloadedStrings #-}

module Core
    ( refToPair
    , pairToRef
    , isPresent
    , insertRefs
    , deleteRefs
    , getRef
    ) where

import qualified Data.Map.Strict as Map
import qualified Types           as T
import qualified Data.Text       as Tx
import Data.Text                        ( Text              )
import Data.List                        ( foldl'            )
import Data.Maybe                       ( mapMaybe          )

refToPair :: T.Ref -> Maybe (Text, T.Entry)
refToPair (T.Ref _ k v  ) = Just (k, v)
refToPair (T.Missing _ _) = Nothing

pairToRef :: FilePath -> (Text, T.Entry) -> T.Ref
pairToRef fp (k, v) = T.Ref fp k v

isPresent :: T.Ref -> Bool
isPresent (T.Ref _ _ _   ) = True
isPresent (T.Missing _ _ ) = False

insertRefs :: T.References -> T.Context -> T.References
-- ^Update a reference map with a list of references.
insertRefs refs = foldl' go refs . mapMaybe refToPair
    where go = flip $ uncurry Map.insert

deleteRefs :: T.References -> T.Context -> T.References
deleteRefs refs = foldl' go refs . fst . unzip . mapMaybe refToPair
    where go = flip Map.delete

getRef :: T.Bibliography -> String -> T.Ref
-- ^Lookup a reference from a bibliography and package as a Ref.
getRef bib x = let key = Tx.pack x
               in  case Map.lookup key ( T.refs bib ) of
                        Nothing -> T.Missing ( T.path bib ) key
                        Just v  -> T.Ref ( T.path bib ) key v
