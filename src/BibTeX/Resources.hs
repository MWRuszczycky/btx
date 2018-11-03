{-# LANGUAGE OverloadedStrings #-}

module BibTeX.Resources
    ( getTemplate
    , supported
    ) where

import qualified Types as T
import Data.Text            ( Text )

-- =============================================================== --
-- Standard references

getTemplate :: Text -> Maybe T.Entry
-- ^Return an empty refernce of the specified type with empty fields.
getTemplate x = lookup x supported

supported :: [ (Text, T.Entry) ]
-- ^Associative list of all supported reference types.
supported = [ ( "article",      article      )
            , ( "book",         book         )
            , ( "incollection", incollection )
            , ( "inbook",       inbook       )
            , ( "phdthesis",    phdthesis    )
            , ( "manual",       manual       )
            , ( "blank",        blank        )
            ]

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
