{-# LANGUAGE OverloadedStrings #-}

module CoreIO
    ( readOrMakeFile
    , readFileExcept
    , writeFileExcept
    , getDoi
    , runExternal
    ) where

import qualified Types               as T
import qualified Data.Text.IO        as Tx
import qualified Data.Text           as Tx
import qualified Data.Text.Encoding  as Tx
import qualified Network.Wreq        as Wreq
import Data.ByteString.Lazy.Internal         ( ByteString          )
import Data.ByteString.Lazy                  ( toStrict            )
import Data.Bifunctor                        ( bimap               )
import Lens.Micro                            ( (^.), (.~), (&)     )
import Data.Text                             ( Text                )
import System.Process                        ( callProcess         )
import System.Directory                      ( removeFile          )
import System.IO.Temp                        ( emptyTempFile       )
import System.IO.Error                       ( isDoesNotExistError )
import Formatting                            ( refToBibtex         )
import BibTeX.Parser                         ( parseRef            )
import Control.Monad.Except                  ( ExceptT (..)
                                             , catchError
                                             , liftEither          )
import Control.Monad.Trans                   ( liftIO              )
import Control.Exception                     ( try, IOException
                                             , SomeException
                                             , displayException
                                             , catch               )

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
runExternal p (T.Missing fp k e) = return $ T.Missing fp k e
runExternal p (T.Ref fp k v   )  = do
    temp <- liftIO . emptyTempFile "." . Tx.unpack $ k
    writeFileExcept temp ( refToBibtex k v )
    callProcExcept p $ [temp]
    content <- readFileExcept temp
    liftIO . removeFile $ temp
    liftEither . bimap ("Cannot parse: " ++) id . parseRef fp $ content

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
