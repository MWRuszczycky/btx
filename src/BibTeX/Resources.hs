{-# LANGUAGE OverloadedStrings #-}

module BibTeX.Resources
    ( genericKey
    , genKeyNumber
    , supported
    , templates
    ) where

import qualified Data.Text       as Tx
import qualified Data.Map.Strict as Map
import qualified Types           as T
import Data.Text                        ( Text )

-- =============================================================== --
-- Standard references

supported :: [ (String, T.Entry) ]
-- ^Associative list of all supported reference types.
supported = [ ( "article",      article      )
            , ( "book",         book         )
            , ( "incollection", incollection )
            , ( "inbook",       inbook       )
            , ( "phdthesis",    phdthesis    )
            , ( "manual",       manual       )
            , ( "blank",        blank        )
            ]

genKeyNumber :: T.Bibliography -> Int
genKeyNumber (T.Bibliography _ rs) = go 0
    where go n | Map.member (numGenKey n) rs = go $ n + 1
               | otherwise                   = n

genericKey :: Text
genericKey = "new_key_"

templates :: Int -> [String] -> T.Context
templates n = foldr go [] . zip [n .. ]
    where go (n,x) = (:) ( T.Ref "new-entry" (numGenKey n) (getTemplate x) )

-- Helpers

getTemplate :: String -> T.Entry
getTemplate x = maybe blank id . lookup x $ supported

numGenKey :: Int -> Text
numGenKey n = genericKey <>  ( Tx.pack . show ) n

---------------------------------------------------------------------
-- Entry templates

blank :: T.Entry
blank = T.Entry {
      T.theType  = "blank"
    , T.comments = []
    , T.fields   = [ ( "author",  "" )
                   , ( "title",   "" )
                   , ( "year",    "" )
                   , ( "pages",   "" )
                   , ( "note",    "" )
                   ] }

article :: T.Entry
article = T.Entry {
      T.theType  = "article"
    , T.comments = []
    , T.fields   = [ ( "author",  "" )
                   , ( "title",   "" )
                   , ( "year",    "" )
                   , ( "journal", "" )
                   , ( "volume",  "" )
                   , ( "number",  "" )
                   , ( "pages",   "" )
                   , ( "month",   "" )
                   , ( "note",    "" )
                   ] }

book :: T.Entry
book = T.Entry {
      T.theType  = "book"
    , T.comments = []
    , T.fields   = [ ( "author",    "" )
                   , ( "title",     "" )
                   , ( "publisher", "" )
                   , ( "address",   "" )
                   , ( "year",      "" )
                   , ( "edition",   "" )
                   , ( "volume",    "" )
                   , ( "series",    "" )
                   , ( "pages",     "" )
                   , ( "number",    "" )
                   , ( "month",     "" )
                   , ( "note",      "" )
                   ] }

incollection :: T.Entry
incollection = T.Entry {
      T.theType  = "incollection"
    , T.comments = []
    , T.fields   = [ ( "author",    "" )
                   , ( "editor",    "" )
                   , ( "title",     "" )
                   , ( "booktitle", "" )
                   , ( "chapter",   "" )
                   , ( "pages",     "" )
                   , ( "publisher", "" )
                   , ( "address",   "" )
                   , ( "year",      "" )
                   , ( "volume",    "" )
                   , ( "number",    "" )
                   , ( "series",    "" )
                   , ( "type",      "" )
                   , ( "edition",   "" )
                   , ( "month",     "" )
                   , ( "note",      "" )
                   ] }

inbook :: T.Entry
inbook = T.Entry {
      T.theType  = "inbook"
    , T.comments = []
    , T.fields   = [ ( "author",    "" )
                   , ( "editor",    "" )
                   , ( "title",     "" )
                   , ( "chapter",   "" )
                   , ( "pages",     "" )
                   , ( "publisher", "" )
                   , ( "address",   "" )
                   , ( "year",      "" )
                   , ( "volume",    "" )
                   , ( "number",    "" )
                   , ( "series",    "" )
                   , ( "type",      "" )
                   , ( "edition",   "" )
                   , ( "month",     "" )
                   , ( "note",      "" )
                   ] }

phdthesis :: T.Entry
phdthesis = T.Entry {
      T.theType  = "phdthesis"
    , T.comments = []
    , T.fields   = [ ( "author",  "" )
                   , ( "title",   "" )
                   , ( "school",  "" )
                   , ( "year",    "" )
                   , ( "type",    "" )
                   , ( "address", "" )
                   , ( "month",   "" )
                   , ( "note",    "" )
                   ] }

manual :: T.Entry
manual = T.Entry {
      T.theType  = "manual"
    , T.comments = []
    , T.fields   = [ ( "author",    "" )
                   , ( "title",     "" )
                   , ( "publisher", "" )
                   , ( "address",   "" )
                   , ( "year",      "" )
                   ] }
