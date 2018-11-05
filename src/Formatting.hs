{-# LANGUAGE OverloadedStrings #-}

module Formatting
    ( refToBibtex
    , bibToBibtex
    , formatRef
    , summarize
    , summarizeAllEntries
    , summarizeEntries
    -- Error messaging
    , argInvalidErr
    , cmdInvalidErr
    , renameErr
    -- General help formatting
    , formatHelp
    ) where

import qualified Data.Text          as Tx
import qualified Data.Map.Strict    as Map
import qualified Types              as T
import Data.Text                            ( Text          )
import Data.List                            ( intercalate
                                            , sort          )

---------------------------------------------------------------------
-- Summarizing bibilographies

summarize :: T.BtxState -> T.Context -> Text
summarize s rs = Tx.intercalate "\n" x
    where x = [ "\nBibliographies:"
              , "  working: " <> summarizeBib ( Just . T.inBib $ s )
              , "  import:  " <> summarizeBib ( T.fromBib s)
              , "  export:  " <> summarizeBib ( T.toBib s )
              , summarizeContext rs
              ]

summarizeBib :: Maybe T.Bibliography -> Text
summarizeBib Nothing  = "unset"
summarizeBib (Just b) = let n = Map.size . T.refs $ b
                        in  Tx.pack (T.path b)
                            <> " has " <> Tx.pack (show n)
                            <> " total entries"

summarizeContext :: T.Context -> Text
summarizeContext [] = "The context is currently empty."
summarizeContext rs = "Context:\n" <> Tx.intercalate "\n" x
    where x = map ( Tx.append "  " . summarizeRef ) $ rs

summarizeRef :: T.Ref -> Text
summarizeRef (T.Ref fp k v  ) = k <> " from " <> Tx.pack fp
summarizeRef (T.Missing fp k) = "The entry " <> k <> " from "
                                <> Tx.pack fp <> " is missing."

summarizeAllEntries :: T.Bibliography -> Text
summarizeAllEntries bib
    | null xs   = "No entries to list."
    | otherwise = Tx.intercalate "\n" xs
    where xs = map summarizeEntry . Map.toList . T.refs $ bib

summarizeEntries :: T.Bibliography -> Text -> Text
summarizeEntries bib x
    | null xs   = "No entries matching " <> x <> "..."
    | otherwise = Tx.intercalate "\n" xs
    where rs     = T.refs bib
          xs     = map summarizeEntry . Map.toList . Map.filterWithKey go $ rs
          go k v = Tx.take (Tx.length x) k == x

summarizeEntry :: (Text, T.Entry) -> Text
summarizeEntry (k, v) = kt <> title
    where pars x = " (" <> x <> ") "
          kt     = k <> ": " <> T.theType v <> ", "
          room   = 80 - Tx.length kt
          go x   | room < Tx.length x = Tx.take (room - 2) x <> ".."
                 | otherwise          = x
          title  = maybe " no title" go . lookup "title" . T.fields $ v

---------------------------------------------------------------------
-- Conversion to BibTeX format

bibToBibtex :: T.Bibliography -> Text
-- ^Generates a text representation of the bibliography.
bibToBibtex = Tx.intercalate "\n\n"
              . Map.elems
              . Map.mapWithKey refToBibtex
              . T.refs

refToBibtex :: Text -> T.Entry -> Text
-- ^Generates a text representation of a bibliography reference.
refToBibtex k v  = Tx.concat [ key, fields, close, coms ]
    where key    = Tx.concat [ "@", T.theType v, "{", k, ",\n" ]
          fields = Tx.intercalate ",\n" . map fieldToBibtex $ T.fields v
          coms   = Tx.intercalate "\n" . map commentToBibtex $ T.comments v
          close  = if Tx.null coms then "\n}" else "\n}\n"

commentToBibtex :: Text -> Text
-- ^Generates a string representation of a comment line.
commentToBibtex = Tx.append "% "

fieldToBibtex :: T.Field -> Text
-- ^Generates a text representation of a bibliography entry field.
fieldToBibtex (k, v) = Tx.concat [ "    ", k, " = ", "{", v, "}" ]

---------------------------------------------------------------------
-- Pretty print references

formatRef :: T.Ref -> Text
-- ^Represent a Ref value as pretty-printed text.
formatRef (T.Missing fp k) = "There is no entry " <> k <> " in " <> Tx.pack fp
formatRef (T.Ref fp k v  ) = Tx.concat x
    where x = [ k <> " in " <> Tx.pack fp <> "\n"
              , formatFields . T.fields $ v
              , if length (T.comments v) > 0
                   then formatComments . T.comments $ v
                   else Tx.empty
              ]

formatFields :: [T.Field] -> Text
-- ^Format the field key-value pairs for pretty-printing.
formatFields xs = Tx.intercalate "\n" . map ( formatPair n ) $ xs
    where n = max 7 . maximum . map Tx.length . fst . unzip $ xs

formatComments :: [Text] -> Text
-- ^Format the comments for pretty-printing.
formatComments xs = let ys = zip (repeat "comment") xs
                    in  Tx.append "\n"
                        . Tx.intercalate "\n"
                        . map ( formatPair 7 )
                        $ ys

formatPair :: Int -> (Text, Text) -> Text
-- ^Take a key value pair, and pretty print with an overhand and a
-- fixed indent for the value text.
formatPair n (x,y) = overHang 80 (n+4)
                     $ "  " <> padRight (n+1) ( x <> ":" ) <> " " <> y

padRight :: Int -> Text -> Text
-- ^Add patting after a text so the total length is n.
padRight n x = x <> Tx.replicate ( n - Tx.length x ) " "

overHang :: Int -> Int -> Text -> Text
-- ^Generate k-overhangs for n-length lines.
overHang n k x
    | Tx.null x = Tx.empty
    | otherwise = Tx.intercalate ind $ p : go s
    where (p,s) = Tx.splitAt n x
          ind   = "\n" <> Tx.replicate k " "
          go xs | Tx.null xs = []
                | otherwise  = ( \ (x,y) -> x : go y ) . Tx.splitAt (n-k) $ xs

---------------------------------------------------------------------
-- Error messages

argInvalidErr :: String -> String -> T.ErrString
argInvalidErr c a = "Invalid argument for " ++ c ++ ": " ++ a

cmdInvalidErr :: String -> T.ErrString
cmdInvalidErr = (++) "Invalid command: "

renameErr :: Int -> Int -> T.ErrString
renameErr n r = intercalate "\n" es
    where es = [ "The entries cannot be renamed, because the number of"
               , "entries currently in the context (" ++ show r
                  ++ ") does not match"
               , "the number of new names supplied (" ++ show n ++ ")." ]

---------------------------------------------------------------------
-- General help formatting

padRightStr :: Int -> String -> String
padRightStr n x = x ++ replicate (n - length x) ' '

formatHelp :: [String] -> String
formatHelp xs = intercalate "\n" . map go $ xs'
    where xs'      = map ( break (== ':') ) . sort $ xs
          n        = maximum . map ( length . fst ) $ xs'
          go (c,s) = padRightStr n c ++ s
