{-# LANGUAGE OverloadedStrings #-}

module Formatting
    ( refToBibtex
    , bibToBibtex
    , formatRef
    , summarize
    ) where

import qualified Data.Text          as Tx
import qualified Data.Map.Strict    as Map
import qualified Types              as T

---------------------------------------------------------------------
-- Summarizing bibilographies

summarize :: T.Bibliography -> Tx.Text
summarize b = Tx.pack (T.path b) <> "\nTotal entries: " <> Tx.pack (show n)
    where n = Map.size . T.refs $ b

---------------------------------------------------------------------
-- Conversion to BibTeX format

bibToBibtex :: T.Bibliography -> Tx.Text
-- ^Generates a text representation of the bibliography.
bibToBibtex = Tx.intercalate "\n" . Map.elems . Map.mapWithKey go . T.refs
    where go k r = ( refToBibtex (k,r) ) <> "\n"

refToBibtex :: T.Ref -> Tx.Text
-- ^Generates a text representation of a bibliography reference.
refToBibtex (k, v) = Tx.concat [ key, fields, close, comments ]
    where key      = Tx.concat [ "@", T.theType v, "{", k, ",\n" ]
          fields   = Tx.intercalate ",\n" . map fieldToBibtex $ T.fields v
          comments = Tx.intercalate "\n" . map commentToBibtex $ T.comments v
          close    = if Tx.null comments then "\n}" else "\n}\n"

commentToBibtex :: Tx.Text -> Tx.Text
-- ^Generates a string representation of a comment line.
commentToBibtex = Tx.append "% "

fieldToBibtex :: T.Field -> Tx.Text
-- ^Generates a text representation of a bibliography entry field.
fieldToBibtex (k, v) = Tx.concat [ "    ", k, " = ", "{", v, "}" ]

---------------------------------------------------------------------
-- Pretty print references

formatRef :: T.Ref -> Tx.Text
-- ^Represent a Ref value as pretty-printed text.
formatRef (k, r) = Tx.concat [ k, "\n",  fields, comments ]
    where fields   = formatFields . T.fields $ r
          comments = if length (T.comments r) > 0
                        then formatComments . T.comments $ r
                        else Tx.empty

formatFields :: [T.Field] -> Tx.Text
-- ^Format the field key-value pairs for pretty-printing.
formatFields xs = Tx.intercalate "\n" . map ( formatPair n ) $ xs
    where n = max 7 . maximum . map Tx.length . fst . unzip $ xs

formatComments :: [Tx.Text] -> Tx.Text
-- ^Format the comments for pretty-printing.
formatComments xs = let ys = zip (repeat "comment") xs
                    in  Tx.append "\n"
                        . Tx.intercalate "\n"
                        . map ( formatPair 7 )
                        $ ys

formatPair :: Int -> (Tx.Text, Tx.Text) -> Tx.Text
-- ^Take a key value pair, and pretty print with an overhand and a
-- fixed indent for the value text.
formatPair n (x,y) = overHang 80 (n+4)
                     $ "  " <> padRight (n+1) ( x <> ":" ) <> " " <> y

padRight :: Int -> Tx.Text -> Tx.Text
-- ^Add patting after a text so the total length is n.
padRight n x = x <> Tx.replicate ( n - Tx.length x ) " "

overHang :: Int -> Int -> Tx.Text -> Tx.Text
-- ^Generate k-overhangs for n-length lines.
overHang n k x
    | Tx.null x = Tx.empty
    | otherwise = Tx.intercalate ind $ p : go s
    where (p,s) = Tx.splitAt n x
          ind   = "\n" <> Tx.replicate k " "
          go xs | Tx.null xs = []
                | otherwise  = ( \ (x,y) -> x : go y ) . Tx.splitAt (n-k) $ xs
