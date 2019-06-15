{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Formatting
    ( -- Bibliography formatting
      refToBibtex
    , bibToBibtex
    , listEntry
    , viewRef
    , viewRefTex
    , summarize
      -- Styling text and style maps
    , noStyles
    , defaultStyles
    , style
    ) where

-- =============================================================== --
-- DSL for converting bibliography information to readable output
-- =============================================================== --

import qualified System.Console.ANSI as Ans
import qualified Data.Text           as Tx
import qualified Data.Map.Strict     as Map
import qualified Model.Core.Types    as T
import Data.Text                            ( Text                )
import Data.List                            ( foldl'              )
import System.Console.ANSI.Types            ( Color (..)
                                            , ColorIntensity (..) )

-- =============================================================== --
-- Bibliography formatting

---------------------------------------------------------------------
-- Summarizing state and context

summarize :: [String] -> T.Context -> T.BtxState -> Text
summarize xs rs s
    | null xs   = Tx.intercalate "\n" x
    | otherwise = h <> "\n" <> Tx.intercalate "\n" x
    where h  = Tx.pack . unwords $ "Info:" : xs
          sm = T.styles s
          x  = [ style sm "header" "Bibliographies:"
               , "  working: " <> summarizeBib sm ( Just . T.inBib $ s )
               , "  import:  " <> summarizeBib sm ( T.fromBib s)
               , "  export:  " <> summarizeBib sm ( T.toBib s )
               , summarizeContext sm rs
               ]

summarizeBib :: T.StyleMap -> Maybe T.Bibliography -> Text
summarizeBib _  Nothing  = "unset"
summarizeBib sm (Just b) = let n     = Map.size . T.refs $ b
                               name  = style sm "key" ( Tx.pack (T.path b) )
                               count = style sm "emph" (Tx.pack (show n))
                           in  name <> " has " <> count
                               <> if n == 1 then " entry" else " entries"

summarizeContext :: T.StyleMap -> T.Context -> Text
summarizeContext _ []  = "The context is currently empty."
summarizeContext sm rs = style sm "header" "Context:\n" <> Tx.intercalate "\n" x
    where x = map ( Tx.append "  " . summarizeRef sm ) $ rs

summarizeRef :: T.StyleMap -> T.Ref -> Text
summarizeRef sm (T.Ref     fp k _ ) = style sm "key" k <> " from " <> Tx.pack fp
summarizeRef sm (T.Missing fp k e ) = style sm "warn" ( k <> " is missing" )
                                          <> " from " <> Tx.pack fp
                                          <> " (" <> Tx.pack e <> ")"

---------------------------------------------------------------------
-- Conversion to BibTeX format

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

listEntry :: T.StyleMap -> T.Ref -> Text
-- ^View an entry in abbreviated, list format.
listEntry sm (T.Missing fp k e) = viewMissing sm fp k e
listEntry sm (T.Ref     _  k v) = style sm "key" k <> meta
                                  <> style sm "emph" title
    where meta  = ": " <> T.theType v <> ", "
          room  = 80 - Tx.length k - Tx.length meta
          go x  | room < Tx.length x = Tx.take (room - 2) x <> ".."
                | otherwise          = x
          title = case lookup "title" . T.fields $ v of
                       Nothing -> " <no title field>"
                       Just t  -> if Tx.null t
                                     then " <empty title field>"
                                     else go t

viewRef :: T.StyleMap -> T.Ref -> Text
-- ^Represent a Ref value as pretty-printed text.
viewRef sm (T.Missing fp k e) = viewMissing sm fp k e
viewRef sm (T.Ref     fp k v) = hdr <> viewEntry sm v
    where hdr = style sm "key" k <> " in " <> Tx.pack fp <> "\n"

viewRefTex :: T.StyleMap -> T.Ref -> Text
-- ^Represent a Ref value in BibTeX format.
viewRefTex sm (T.Missing fp k e) = viewMissing sm fp k e
viewRefTex _  (T.Ref     _  k v) = refToBibtex k v

viewMissing :: T.StyleMap -> FilePath -> T.Key -> T.ErrString -> Text
-- ^Formats a missing entry as Text for viewing.
viewMissing sm fp k e = Tx.concat [ style sm "warn" $ "Missing: " <> k
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
        keyTxt   = "  " <> padRight (n+2) (x <> ": ")
        fieldTxt = overHang (lineLen - n - 4) (n+4) y
        toEmph   = [ "type", "title", "booktitle", "chapter", "journal" ]
    in  style sm "field" keyTxt <> if elem x toEmph
                                      then style sm "emph" fieldTxt
                                      else fieldTxt

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

-- =============================================================== --
-- Styling text for terminal display

---------------------------------------------------------------------
-- Style maps

noStyles :: T.StyleMap
noStyles = Map.empty

defaultStyles :: T.StyleMap
defaultStyles = Map.fromList cs
    where cs = [ ( "header", styleString True  Dull Blue   )
               , ( "emph",   styleString False Dull Yellow )
               , ( "key",    styleString True  Dull Green  )
               , ( "warn",   styleString True  Dull Red    )
               , ( "field",  styleString False Dull Cyan   )
               ]

---------------------------------------------------------------------
-- Styling helper functions

style :: T.StyleMap -> Text -> Text -> Text
style sm = maybe id id . flip Map.lookup sm

styleString :: Bool -> Ans.ColorIntensity -> Ans.Color -> Text -> Text
styleString b i c s = start <> s <> stop
    where stop  = Tx.pack . Ans.setSGRCode $ [ Ans.Reset ]
          start = Tx.pack . Ans.setSGRCode $ code
          code  | not b     = [ Ans.SetColor Ans.Foreground i c ]
                | otherwise = [ Ans.SetColor Ans.Foreground i c
                              , Ans.SetConsoleIntensity Ans.BoldIntensity]
