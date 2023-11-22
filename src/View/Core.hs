{-# LANGUAGE OverloadedStrings #-}

module View.Core
    ( view
    , write
    , writeAs
    , writeLn
    , writeLnAs
    , newline
    , sepWith
-- Text versus String
    , tshow
-- Formatting lines and blocks of text
    , addEscapes
    , dQuote
    , padRight
    , overHang
    , breakToFit
-- Decoding and formatting ByteStrings
    , toAscii
-- Styles
    , noStyles
    , defaultStyles
    , style
    , styleText
    ) where

import qualified System.Console.ANSI       as Ans
import qualified Data.ByteString           as BS
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Data.Text                 as Tx
import qualified Data.Text.Encoding        as Tx
import           Data.List                        ( foldl', intersperse )
import           Data.Text                        ( Text                )
import           Data.Monoid                      ( Endo (..), appEndo  )
import           Control.Monad.State              ( get                 )
import           Control.Monad.Reader             ( runReader, asks     )
import           Control.Monad.Writer             ( execWriterT, tell   )
import           System.Console.ANSI.Types        ( Color (..)
                                                  , ColorIntensity (..) )

-- =============================================================== -- 
-- ViewMonad

view :: T.ViewMonad a -> T.BtxMonad Text
view v = get >>= pure . Tx.concat . flip appEndo [] . run
    where run btx = flip runReader (T.config btx) . execWriterT $ v

write :: Text -> T.ViewMonad ()
write x = tell $ Endo ( [x] <> )

writeAs :: Text -> Text -> T.ViewMonad ()
writeAs s x = do
    sm <- asks T.cStyles
    write $ style sm s x

writeLn :: Text -> T.ViewMonad ()
writeLn x = write x *> newline

writeLnAs :: Text -> Text -> T.ViewMonad ()
writeLnAs s x = writeAs s x *> newline

newline :: T.ViewMonad ()
newline = write "\n"

dQuote :: Text -> T.ViewMonad ()
dQuote t = write . Tx.concat $ ["\"", t, "\""]

sepWith :: T.ViewMonad () -> [T.ViewMonad ()] -> T.ViewMonad ()
sepWith sep = sequence_ . intersperse sep

-- =============================================================== --
-- Text versus String

tshow :: Show a => a -> Text
tshow = Tx.pack . show

-- =============================================================== -- 
-- Formatting lines and blocks of text

addEscapes :: Text -> Text
-- ^Add escapes for quotes and backslashes:
-- """ converted to "\""
-- "\" converted to "\\"
addEscapes = Tx.concatMap go
    where go '"'  = "\\\""
          go '\\' = "\\\\"
          go x    = Tx.singleton x

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
-- Decoding and formatting ByteStrings

toAscii :: BS.ByteString -> Text
-- ^Converts a bytestring to text changing all non-ascii characters
-- to bracketed question marks.
toAscii = Tx.concatMap go . Tx.decodeUtf8
    where go c | fromEnum c < 128 = Tx.singleton c
               | otherwise        = "[?]"

-- =============================================================== --
-- Styling text for terminal display

---------------------------------------------------------------------
-- Style maps

noStyles :: T.StyleMap
noStyles = Map.empty

defaultStyles :: T.StyleMap
defaultStyles = Map.fromList
    [ ( "header", styleText True  Dull Blue   )
    , ( "emph",   styleText False Dull Yellow )
    , ( "key",    styleText True  Dull Green  )
    , ( "warn",   styleText True  Dull Red    )
    , ( "field",  styleText False Dull Cyan   )
    , ( "short",  styleText True  Dull Cyan   )
    ]

---------------------------------------------------------------------
-- Styling helper functions

style :: T.StyleMap -> Text -> Text -> Text
style sm = maybe id id . flip Map.lookup sm

styleText :: Bool -> Ans.ColorIntensity -> Ans.Color -> Text -> Text
styleText b i c s = start <> s <> stop
    where stop  = Tx.pack . Ans.setSGRCode $ [ Ans.Reset ]
          start = Tx.pack . Ans.setSGRCode $ code
          code  | not b     = [ Ans.SetColor Ans.Foreground i c ]
                | otherwise = [ Ans.SetColor Ans.Foreground i c
                              , Ans.SetConsoleIntensity Ans.BoldIntensity]
