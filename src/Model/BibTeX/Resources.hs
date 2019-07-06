{-# LANGUAGE OverloadedStrings #-}

module Model.BibTeX.Resources
    ( genericKey
    , supported
    , templates
    , uniqueKeys
    ) where

-- =============================================================== --
-- Templates and handlers for adding new bibliography entries
-- =============================================================== --

import qualified Data.Text        as Tx
import qualified Data.Map.Strict  as Map
import qualified Model.Core.Types as T
import Data.Text                         ( Text )

-- =============================================================== --
-- Standard references

supported :: [ (Text, T.Entry) ]
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

templates :: [(T.Key, String)] -> T.Context
-- ^Create blank templates based on a list of BibTeX entry types and
-- corresponding keys to name them with.
templates = map ( \ (k,x) -> T.Ref "new-entry" k (go x) )
    where go t = maybe misc id . lookup (Tx.pack t) $ supported

uniqueKeys :: T.Bibliography -> [T.Key]
-- ^List of key names that are not already in the bibliogrphy.
uniqueKeys (T.Bibliography _ rs _) = filter (not . flip Map.member rs) keys
    where keys = map ((genericKey <>) . Tx.pack . show) [0..]

genericKey :: Tx.Text
-- ^This is defined separately so it fits into help information.
-- The idea is that if the user creates a new reference, they will
-- be able to find it more easily if the help tells them how it will
-- be named (in case they forget to rename it).
genericKey = "new_key_"

---------------------------------------------------------------------
-- Entry templates

article :: T.Entry
article = T.Entry {
      T.theType  = "article"
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
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
    , T.metadata = []
    , T.fields   = [ ( "author",  "" )
                   , ( "title",   "" )
                   , ( "year",    "" )
                   , ( "month",   "" )
                   , ( "note",    "" )
                   ] }
