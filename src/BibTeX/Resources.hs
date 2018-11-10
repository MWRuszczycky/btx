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
supported = [ ( "article",       article       )
            , ( "book",          book          )
            , ( "booklet",       booklet       )
            , ( "conference",    conference    )
            , ( "inbook",        inbook        )
            , ( "incollection",  incollection  )
            , ( "inproceedings", inproceedings )
            , ( "manual",        manual        )
            , ( "mastersthesis", mastersthesis )
            , ( "misc",          misc          )
            , ( "phdthesis",     phdthesis     )
            , ( "proceedings",   proceedings   )
            , ( "techreport",    techreport    )
            , ( "unpublished",   unpublished   )
            ]

genericKey :: Text
-- ^Generic key for new templates.
genericKey = "new_key_"

genKeyNumber :: T.Bibliography -> Int
-- ^Generate a number n so that n appended to genericKey is not a key
-- already used in the bibliography.
genKeyNumber (T.Bibliography _ rs) = go 0
    where go n | Map.member (numGenKey n) rs = go $ n + 1
               | otherwise                   = n

templates :: Int -> [String] -> T.Context
-- ^Create blank templates based on a list of BibTeX entry types.
templates n = foldr go [] . zip [n .. ]
    where go (n,x) = (:) ( T.Ref "new-entry" (numGenKey n) (getTemplate x) )

-- Helpers

getTemplate :: String -> T.Entry
-- ^Create an unkeyed BibTeX entry template with the specified type.
getTemplate x = maybe misc id . lookup x $ supported

numGenKey :: Int -> Text
-- ^Helper function for numbering generic keys.
numGenKey n = genericKey <>  ( Tx.pack . show ) n

---------------------------------------------------------------------
-- Entry templates

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

booklet :: T.Entry
booklet = T.Entry {
      T.theType  = "booklet"
    , T.comments = []
    , T.fields = [ ( "author",       "" )
                 , ( "title",        "" )
                 , ( "howpublished", "" )
                 , ( "address",      "" )
                 , ( "year",         "" )
                 , ( "month",        "" )
                 , ( "note",         "" )
                 ] }

conference :: T.Entry
conference = T.Entry {
      T.theType  = "conference"
    , T.comments = []
    , T.fields   = [ ( "author",    "" )
                   , ( "title",     "" )
                   , ( "booktitle", "" )
                   , ( "editor",    "" )
                   , ( "volume",    "" )
                   , ( "number",    "" )
                   , ( "series",    "" )
                   , ( "pages",     "" )
                   , ( "address",   "" )
                   , ( "year",      "" )
                   , ( "month",     "" )
                   , ( "publisher", "" )
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

inproceedings :: T.Entry
inproceedings = T.Entry {
      T.theType  = "inproceedings"
    , T.comments = []
    , T.fields   = [ ( "author",       "" )
                   , ( "title",        "" )
                   , ( "booktitle",    "" )
                   , ( "editor",       "" )
                   , ( "volume",       "" )
                   , ( "number",       "" )
                   , ( "series",       "" )
                   , ( "pages",        "" )
                   , ( "address",      "" )
                   , ( "organization", "" )
                   , ( "publisher",    "" )
                   , ( "year",         "" )
                   , ( "month",        "" )
                   , ( "note",         "" )
                   ] }

manual :: T.Entry
manual = T.Entry {
      T.theType  = "manual"
    , T.comments = []
    , T.fields   = [ ( "author",        "" )
                   , ( "title",         "" )
                   , ( "author",        "" )
                   , ( "organization",  "" )
                   , ( "address",       "" )
                   , ( "edition",       "" )
                   , ( "year",          "" )
                   , ( "month",         "" )
                   , ( "note",          "" )
                   ] }

mastersthesis :: T.Entry
mastersthesis = T.Entry {
      T.theType  = "mastersthesis"
    , T.comments = []
    , T.fields   = [ ( "author",        "" )
                   , ( "title",         "" )
                   , ( "school",        "" )
                   , ( "type",          "" )
                   , ( "address",       "" )
                   , ( "year",          "" )
                   , ( "month",         "" )
                   , ( "note",          "" )
                   ] }

misc :: T.Entry
misc = T.Entry {
      T.theType  = "misc"
    , T.comments = []
    , T.fields   = [ ( "author",        "" )
                   , ( "title",         "" )
                   , ( "howpublished",  "" )
                   , ( "year",          "" )
                   , ( "month",         "" )
                   , ( "note",          "" )
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

proceedings :: T.Entry
proceedings = T.Entry {
      T.theType  = "proceedings"
    , T.comments = []
    , T.fields   = [ ( "title",        "" )
                   , ( "editor",       "" )
                   , ( "volume",       "" )
                   , ( "number",       "" )
                   , ( "series",       "" )
                   , ( "address",      "" )
                   , ( "organization", "" )
                   , ( "publisher",    "" )
                   , ( "year",         "" )
                   , ( "month",        "" )
                   , ( "note",         "" )
                   ] }

techreport :: T.Entry
techreport = T.Entry {
      T.theType  = "techreport"
    , T.comments = []
    , T.fields   = [ ( "author",      "" )
                   , ( "title",       "" )
                   , ( "year",        "" )
                   , ( "institution", "" )
                   , ( "type",        "" )
                   , ( "number",      "" )
                   , ( "address",     "" )
                   , ( "month",       "" )
                   , ( "note",        "" )
                   ] }

unpublished :: T.Entry
unpublished = T.Entry {
      T.theType  = "unpublished"
    , T.comments = []
    , T.fields   = [ ( "author",  "" )
                   , ( "title",   "" )
                   , ( "year",    "" )
                   , ( "month",   "" )
                   , ( "note",    "" )
                   ] }
