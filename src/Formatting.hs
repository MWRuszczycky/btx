{-# LANGUAGE OverloadedStrings #-}

module Formatting
    ( refToBibtex
    , bibToBibtex
    , viewRef
    , summarize
    , summarizeAllEntries
    , summarizeEntries
    -- Error messaging
    , argInvalidErr
    , cmdInvalidErr
    , renameErr
    , uniqueBibErr
    -- General help formatting
    , formatHelp
    ) where

import qualified Data.Text          as Tx
import qualified Data.Map.Strict    as Map
import qualified Types              as T
import qualified Help               as H
import Data.Text                            ( Text          )
import Data.List                            ( foldl'
                                            , intercalate
                                            , sort          )

---------------------------------------------------------------------
-- Summarizing bibilographies

summarize :: [String] -> T.Context -> T.BtxState -> Text
summarize xs rs s
    | null xs   = "\n" <> Tx.intercalate "\n" x
    | otherwise = h <> "\n" <> Tx.intercalate "\n" x
    where h = Tx.pack . unwords $ "\nInfo:" : xs
          x = [ "Bibliographies:"
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
                            <> if n == 1 then " entry" else " entries"

summarizeContext :: T.Context -> Text
summarizeContext [] = "The context is currently empty."
summarizeContext rs = "Context:\n" <> Tx.intercalate "\n" x
    where x = map ( Tx.append "  " . summarizeRef ) $ rs

summarizeRef :: T.Ref -> Text
summarizeRef (T.Ref fp k v     ) = k <> " from " <> Tx.pack fp
summarizeRef (T.Missing fp k e ) = "Missing: " <> k <> " from " <> Tx.pack fp
                                   <> " (" <> Tx.pack e <> ")"

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
bibToBibtex b
    | Tx.null h = rs
    | otherwise = h <> "\n\n" <> rs
    where h  = T.header b
          rs = Tx.intercalate "\n"
               . Map.elems
               . Map.mapWithKey refToBibtex
               . T.refs $ b

refToBibtex :: Text -> T.Entry -> Text
-- ^Generates a text representation of a bibliography reference.
refToBibtex k v  = Tx.concat [ key, fields, close, coms <> "\n" ]
    where key    = Tx.concat [ "@", T.theType v, "{", k, ",\n" ]
          fields = Tx.intercalate ",\n" . map fieldToBibtex $ T.fields v
          coms   = Tx.intercalate "\n" . T.metadata $ v
          close  = if Tx.null coms then "\n}" else "\n}\n"

fieldToBibtex :: T.Field -> Text
-- ^Generates a text representation of a bibliography entry field.
fieldToBibtex (k, v) = Tx.concat [ "    ", k, " = ", "{", v, "}" ]

---------------------------------------------------------------------
-- Pretty print references for use with <view> command

viewRef :: T.Ref -> Text
-- ^Represent a Ref value as pretty-printed text.
viewRef (T.Missing fp k e ) = viewMissing fp k e
viewRef (T.Ref     fp k v ) = hdr <> viewEntry v
    where hdr = k <> " in " <> Tx.pack fp <> "\n"

viewMissing :: FilePath -> T.Key -> T.ErrString -> Text
-- ^Formats a missing entry as Text for viewing.
viewMissing fp k e = Tx.concat [ "Missing: " <> k
                               , " from " <> Tx.pack fp
                               , " (" <> Tx.pack e <> ")"
                               ]

viewEntry :: T.Entry -> Text
-- ^Formats a non-missing entry as Text for view.
viewEntry (T.Entry t fs cs) = viewPairs $ ("type", t) : fs' ++ ms
    where fs' = filter ( not . Tx.null . snd ) fs
          ms  = zip (repeat "metadata") cs

viewPairs :: [(Text, Text)] -> Text
-- ^Format the field key-value pairs for pretty-printing.
viewPairs fs
    | length fs' < 2 = Tx.intercalate "\n" $ map ( formatPair 0 ) fs' ++ msg
    | otherwise      = Tx.intercalate "\n" . map ( formatPair n ) $ fs'
    where fs' = filter ( not . Tx.null . snd ) fs
          n   = maximum . map Tx.length . fst . unzip $ fs'
          msg = ["  No non-empty fields or metadata"]

formatPair :: Int -> (Text, Text) -> Text
-- ^Take a key value pair, and pretty print reserving n characters
-- for the key preceded by 2 spaces and followed by a colon and a
-- space and then the field. Field lines then all line up using a
-- fixed indent of 2 + n + 2 spaces.
formatPair n (x,y) = let lineLength = 80 -- characters
                         keyTxt     = "  " <> padRight (n+2) (x <> ": ")
                         fieldTxt   = overHang (lineLength - n - 4) (n+4) y
                     in  keyTxt <> fieldTxt

padRight :: Int -> Text -> Text
-- ^Add padding spaces after a text so the total length is n.
padRight n x = x <> Tx.replicate ( n - Tx.length x ) " "

overHang :: Int -> Int -> Text -> Text
-- ^Generate k-overhangs for n-length lines.
overHang n k t
    | Tx.null t = Tx.empty
    | otherwise = Tx.intercalate indent $ breakToFit n t
    where indent = "\n" <> Tx.replicate k " "

breakToFit :: Int -> Text -> [Text]
-- ^Break text up so that it all fits on a line of n characters.
-- Breaks are placed between words unless the word is too long to fit
-- on a single line, in which case it is hyphenated.
breakToFit n x
    | n < 1     = []
    | n == 1    = map Tx.singleton . Tx.unpack $ x
    | otherwise = reverse . foldl' go [] . Tx.words $ x
    where go []     w | Tx.null w  = []   -- nothing to do
                      | Tx.null w2 = [w1] -- everything fits
                      | otherwise  = go [] w4 ++ [ w3 <> "-" ]
                      where (w1,w2) = Tx.splitAt n w
                            (w3,w4) = Tx.splitAt ( n - 1 ) w
          go (t:ts) w | Tx.null w  = []                    -- nothing to do
                      | Tx.null w2 = (t <> " " <> w1) : ts -- everthing fits
                      | otherwise  = go [] w ++ (t : ts)   -- doesn't fit
                      where (w1,w2) = Tx.splitAt ( n - Tx.length t - 1 ) w

---------------------------------------------------------------------
-- Error messages

argInvalidErr :: String -> String -> T.ErrString
argInvalidErr c a = "Invalid argument for " ++ c ++ ": " ++ a ++ ".\n"

cmdInvalidErr :: String -> T.ErrString
cmdInvalidErr c = "Invalid command: " ++ "<" ++ c ++ ">.\n"

renameErr :: Int -> Int -> T.ErrString
renameErr n r = unlines es
    where es = [ "The entries cannot be renamed, because the number of"
               , "entries currently in the context (" ++ show r
                  ++ ") does not match"
               , "the number of new names supplied (" ++ show n ++ ")."
               ]

uniqueBibErr :: FilePath -> T.ErrString
uniqueBibErr fp = unlines es
    where es = [ "Cannot find a unique default .bib file in the current"
               , "directory (" ++ fp ++ ")"
               , "(Try: btx help in)"
               ]

---------------------------------------------------------------------
-- General help formatting

padRightStr :: Int -> String -> String
padRightStr n x = x ++ replicate (n - length x) ' '

summarizeCommands :: [String] -> String
summarizeCommands xs = intercalate "\n" . map go $ xs'
    where xs'      = map ( break (== ':') ) . sort $ xs
          n        = maximum . map ( length . fst ) $ xs'
          go (c,s) = padRightStr n c ++ s

formatHelp :: [String] -> String
formatHelp xs = unlines hs
    where hs = [ H.helpStrHeader
               , "\n-- usage --------------------------------------------------"
                 ++ replicate 20 '-'
               , "btx [run [FILE-PATH] | help [COMMAND] | version] [SCRIPT]"
               , "\n-- btx directives -----------------------------------------"
                 ++ replicate 20 '-'
               , H.directiveHelpStr
               , "\n-- btx keyword summaries ----------------------------------"
                 ++ replicate 20 '-'
               , H.keywordHelpStr
               , "\n-- btx command summaries ----------------------------------"
                 ++ replicate 20 '-'
               , summarizeCommands xs
               , "\n-- copying ------------------------------------------------"
                 ++ replicate 20 '-'
               , H.helpStrFooter
               ]
