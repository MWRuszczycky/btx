{-# LANGUAGE OverloadedStrings #-}

module CoreIO
    ( readOrMakeFile
    , readFileExcept
    , writeFileExcept
    , getDoi
    , runExternal
    ) where

import qualified Data.Text.IO        as Tx
import qualified Data.Text           as Tx
import qualified Data.Text.Encoding  as Tx
import qualified Network.Wreq        as Wreq
import qualified Types               as T
import Data.Char                             ( isSpace             )
import Data.ByteString.Lazy.Internal         ( ByteString          )
import Data.ByteString.Lazy                  ( toStrict            )
import Data.Bifunctor                        ( bimap               )
import Lens.Micro                            ( (^.), (.~), (&)     )
import Data.Text                             ( Text                )
import System.Process                        ( callProcess         )
import System.Directory                      ( removeFile          )
import System.IO.Temp                        ( emptyTempFile       )
import System.IO.Error                       ( isDoesNotExistError )
import Control.Monad.Trans                   ( liftIO              )
import Control.Monad.Except                  ( ExceptT (..)
                                             , throwError
                                             , catchError
                                             , liftEither          )
import Control.Exception                     ( try, IOException
                                             , SomeException
                                             , displayException
                                             , catch               )
import Formatting                            ( refToBibtex         )
import BibTeX.Parser                         ( parseRef            )

-- =============================================================== --
-- IO with exception handling

---------------------------------------------------------------------
-- Reading and writing files

readOrMakeFile :: FilePath -> T.ErrMonad Text
-- ^Try to read a file, and if it does not exist, then create it.
readOrMakeFile fp = ExceptT $ do
    liftIO . catch ( Right <$> Tx.readFile fp ) $ hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr e | isDoesNotExistError e = return . Right $ Tx.empty
                    | otherwise             = return . Left . show $ e

readFileExcept :: FilePath -> T.ErrMonad Text
readFileExcept fp = ExceptT $ do
    liftIO . catch ( Right <$> Tx.readFile fp ) $ hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr = return . Left . show

writeFileExcept :: FilePath -> Text -> T.ErrMonad ()
writeFileExcept fp x = ExceptT $ do
    liftIO . catch ( Right <$> Tx.writeFile fp x ) $ hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = return . Left . show

---------------------------------------------------------------------
-- Handling external processes

callProcExcept :: FilePath -> [String] -> T.ErrMonad ()
callProcExcept p args = ExceptT $ do
    liftIO . catch ( Right <$> callProcess p args ) $ hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = return . Left . displayException

runExternal :: String -> T.Ref -> T.ErrMonad T.Ref
runExternal p (T.Missing fp k e)  = return $ T.Missing fp k e
runExternal p r@(T.Ref fp k v   ) = do
    let toEdit = Tx.append editIntro . refToBibtex k $ v
    temp   <- liftIO . emptyTempFile "." . Tx.unpack $ k <> ".bib"
    result <- catchError ( parseLoop p fp temp toEdit )
                         ( \ e -> liftIO (removeFile temp) >> throwError e )
    liftIO . removeFile $ temp
    return . maybe r id $ result

parseLoop :: String -> FilePath -> FilePath -> Text -> T.ErrMonad (Maybe T.Ref)
parseLoop p fp temp xs = do
    writeFileExcept temp $ xs
    callProcExcept p [temp]
    content <- readFileExcept temp
    if quit content
       then return Nothing
       else case parseRef fp content of
                 Left err -> parseLoop p fp temp ( addErrMsg content )
                 Right r  -> return . Just $ r

editIntro :: Text
editIntro = Tx.intercalate "\n" hs
    where hs = [ "\n% Please edit the BibTeX entry below."
               , "% Leading comments will be stripped."
               , "% Following comments will be stripped if they are separated"
               , "% from the reference by line breaks."
               , "\n% TO ABORT THE EDIT, delete the full entry, save and quit."
               , "\n\n"
               ]

addErrMsg :: Text -> Text
addErrMsg = Tx.append (hs <> editIntro) . Tx.dropWhile ( /= '@' )
    where hs = "\n% PARSE ERROR: Check the BibTeX syntax and try again.\n"

quit :: Text -> Bool
quit = null . filter isRef . map (Tx.dropWhile isSpace) . Tx.lines
    where isRef x = not $ Tx.null x || Tx.head x == '%'

---------------------------------------------------------------------
-- Interfacing with the internet

type WreqResponse = Wreq.Response ByteString

getDoi :: String -> T.ErrMonad T.Ref
-- ^Attempt to download a BibTeX reference from a doi. Retrieving
-- references with the indicated Accept header and doi address should
-- result in braced rather than quoted reference fields.
getDoi doi = do
    let ps = [ "text/bibliography; style=bibtex" ]
        os = Wreq.defaults & Wreq.header "Accept" .~ ps
        ad = "http://dx.doi.org/" ++ doi
    catchError ( tryToConnectWithGet os ad >>= readResponse ad )
               ( return . T.Missing doi "no-key"               )

tryToConnectWithGet :: Wreq.Options -> String -> T.ErrMonad WreqResponse
tryToConnectWithGet ops address = ExceptT $ do
    liftIO . catch ( Right <$> Wreq.getWith ops address ) $ hndlErr
    where hndlErr :: SomeException -> IO ( Either T.ErrString WreqResponse )
          hndlErr _ = return . Left $ "Unable to connect to " ++ address

readResponse :: String -> WreqResponse -> T.ErrMonad T.Ref
-- ^Helper function for reading the response to a get request.
readResponse ad resp
    | code == 200 = liftEither . perr . parseRef ad . fmt $ body
    | otherwise   = liftEither . Left $ errs
    where code = resp ^. Wreq.responseStatus . Wreq.statusCode
          body = resp ^. Wreq.responseBody
          errs = "Cannot access " ++ ad ++ ", error code: " ++ show code
          perr = bimap ("Cannot parse: " ++) id

fmt :: ByteString -> Text
-- ^Converts a bytestring to text changing all non-ascii characters
-- to question marks.
fmt = Tx.reverse . Tx.foldl' addAscii Tx.empty . Tx.decodeUtf8 . toStrict

addAscii :: Text -> Char -> Text
-- ^Add character to text if ascii or add '?' if non-ascii.
addAscii cs c
    | n < 128   = Tx.cons c cs
    | otherwise = ">?<" <> cs
    where n = fromEnum c
