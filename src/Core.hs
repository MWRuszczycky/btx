{-# LANGUAGE OverloadedStrings #-}

module Core
    ( deleteRefs
    , insertRefs
    , isPresent
    , getRef
    , pairToRef
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
