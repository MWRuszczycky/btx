{-# LANGUAGE OverloadedStrings #-}

module View.View
    ( -- Bibliography formatting
      refToBibtex
    , bibToBibtex
    , listEntry
    , viewRef
    , viewRefTex
    , summarize
    ) where

-- =============================================================== --
-- DSL for converting bibliography information to text strings
-- =============================================================== --

import qualified Data.Map.Strict  as Map
import qualified Model.Core.Types as T
import qualified Data.Text        as Tx
import qualified View.Core        as Vc
import           Data.Text               ( Text   )
import           Control.Monad           ( unless )

-- =============================================================== --
-- Bibliography formatting

---------------------------------------------------------------------
-- Summarizing state and context

summarize :: [String] -> T.Context -> T.BtxState -> T.ViewMonad ()
summarize xs rs btx = do
    unless (null xs) . Vc.writeLn . Tx.pack . unwords $ "Info:" : xs
    Vc.writeLnAs "header" "Bibliographies:"
    Vc.write "  working: " *> summarizeBib ( Just . T.inBib $ btx )
    Vc.write "  import:  " *> summarizeBib ( T.fromBib        btx )
    Vc.write "  export:  " *> summarizeBib ( T.toBib          btx )
    summarizeContext rs

summarizeBib :: Maybe T.Bibliography -> T.ViewMonad ()
summarizeBib Nothing  = Vc.writeLn "unset"
summarizeBib (Just b) =  do
    let count = Map.size . T.refs $ b
    Vc.writeAs "key" . Tx.pack . T.path $ b
    Vc.write " has "
    Vc.writeAs "emph" . Vc.tshow $ count
    if count == 1 then Vc.write " entry" else Vc.write " entries"
    Vc.newline

summarizeContext :: T.Context -> T.ViewMonad ()
summarizeContext [] = Vc.write "The context is currently empty."
summarizeContext rs = do
    let indent2 = Vc.write "  "
    Vc.writeLnAs "header" "Context:"
    Vc.sepWith Vc.newline . map ( \ r -> indent2 *> summarizeRef r ) $ rs

summarizeRef :: T.Ref -> T.ViewMonad ()
summarizeRef ( T.Ref    fp k _ ) = do
    Vc.writeAs "key" k
    Vc.write $ " from " <> Tx.pack fp
summarizeRef (T.Missing fp k e ) = do
    Vc.writeAs "warn" k
    Vc.write $ " is missing from " <> Tx.pack fp <> " (" <> Tx.pack e <> ")"

---------------------------------------------------------------------
-- Conversion to BibTeX format
-- There should be no text-styling in these functions, because they
-- are used to write the .bib files.

bibToBibtex :: T.Bibliography -> Text
-- ^Generates a text representation of the bibliography.
bibToBibtex b
    | Tx.null h = rs
    | otherwise = h <> "\n\n" <> rs
    where h         = T.header b
          rs        = Tx.concat . Map.foldrWithKey go [] . T.refs $ b
          go k v [] = [ refToBibtex k v <> "\n" ]
          go k v xs = ( refToBibtex k v <> "\n\n" ) : xs

refToBibtex :: Text -> T.Entry -> Text
-- ^Generates a text representation of a bibliography reference.
refToBibtex k v  = Tx.concat [ key, fields, close, coms ]
    where key    = Tx.concat [ "@", T.theType v, "{", k, ",\n" ]
          fields = Tx.intercalate ",\n" . map fieldToBibtex $ T.fields v
          coms   = Tx.intercalate "\n" . T.metadata $ v
          close  = if Tx.null coms then "\n}" else "\n}\n"

fieldToBibtex :: T.Field -> Text
-- ^Generates a text representation of a bibliography entry field.
fieldToBibtex (k, v) = Tx.concat [ "    ", k, " = ", "{", v, "}" ]

---------------------------------------------------------------------
-- Pretty print references for use with <view> command

-- TODO: Fix this to use the ViewMonad correctly
listEntry :: T.Config -> T.Ref -> T.ViewMonad ()
-- ^View an entry in abbreviated, list format.
listEntry c (T.Missing fp k e) = Vc.write $ viewMissing (T.cStyles c) fp k e
listEntry c (T.Ref     _  k v) = Vc.write $ Vc.style sm "key" k <> meta
                                            <> Vc.style sm "emph" title
    where sm    = T.cStyles c
          meta  = ": " <> T.theType v <> ", "
          room  = 80 - Tx.length k - Tx.length meta
          go x  | room < Tx.length x = Tx.take (room - 2) x <> ".."
                | otherwise          = x
          title = case lookup "title" . T.fields $ v of
                       Nothing -> " <no title field>"
                       Just t  -> if Tx.null t
                                     then " <empty title field>"
                                     else go t

-- TODO: Fix this to use the ViewMonad correctly
viewRef :: T.Config -> T.Ref -> T.ViewMonad ()
-- ^Represent a Ref value as pretty-printed text.
viewRef c (T.Missing fp k e) = Vc.write $ viewMissing (T.cStyles c) fp k e
viewRef c (T.Ref     fp k v) = Vc.write $ hdr <> viewEntry (T.cStyles c) v
    where hdr = Vc.style (T.cStyles c) "key" k <> " in " <> Tx.pack fp <> "\n"

-- TODO: Fix this to use the ViewMonad correctly
viewRefTex :: T.Config -> T.Ref -> T.ViewMonad ()
-- ^Represent a Ref value in BibTeX format.
viewRefTex c (T.Missing fp k e) = Vc.write $ viewMissing sm fp k e
    where sm = T.cStyles c
viewRefTex _ (T.Ref     _  k v) = Vc.write $ refToBibtex k v

viewMissing :: T.StyleMap -> FilePath -> T.Key -> T.ErrString -> Text
-- ^Formats a missing entry as Text for viewing.
viewMissing sm fp k e = Tx.concat [ Vc.style sm "warn" $ "Missing: " <> k
                                  , " from " <> Tx.pack fp
                                  , " (" <> Tx.pack e <> ")"
                                  ]

viewEntry :: T.StyleMap -> T.Entry -> Text
-- ^Formats a non-missing entry as Text for view.
viewEntry sm (T.Entry t fs cs) = viewPairs sm $ ("type", t) : fs' ++ ms
    where fs' = filter ( not . Tx.null . snd ) fs
          ms  = zip (repeat "metadata") cs

viewPairs :: T.StyleMap -> [(Text, Text)] -> Text
-- ^Format the field key-value pairs for pretty-printing.
viewPairs sm fs
    | length fs' < 2 = Tx.intercalate "\n" $ map ( formatPair sm 0 ) fs' ++ msg
    | otherwise      = Tx.intercalate "\n" . map ( formatPair sm n ) $ fs'
    where fs' = filter ( not . Tx.null . snd ) fs
          n   = maximum . map Tx.length . fst . unzip $ fs'
          msg = ["  No non-empty fields or metadata"]

formatPair :: T.StyleMap -> Int -> (Text, Text) -> Text
-- ^Take a key value pair, and pretty print reserving n characters
-- for the key preceded by 2 spaces and followed by a colon and a
-- space and then the field. Field lines then all line up using a
-- fixed indent of 2 + n + 2 spaces.
formatPair sm n (x,y) =
    let lineLen  = 80 -- characters
        keyTxt   = "  " <> Vc.padRight (n+2) (x <> ": ")
        fieldTxt = Vc.overHang (lineLen - n - 4) (n+4) y
        toEmph   = [ "type", "title", "booktitle", "chapter", "journal" ]
    in  Vc.style sm "field" keyTxt <> if elem x toEmph
                                         then Vc.style sm "emph" fieldTxt
                                         else fieldTxt
