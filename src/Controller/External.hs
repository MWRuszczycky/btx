{-# LANGUAGE OverloadedStrings #-}

module Controller.External
    ( getDoi
    , runExternal
    ) where

-- =============================================================== --
-- IO DSL for commands involving the internet and external processes
-- =============================================================== --

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Controller.ErrMonad           as E
import qualified Model.Types                   as T
import qualified Data.Text                     as Tx
import qualified Network.Wreq                  as Wreq
import           Data.ByteString.Lazy                  ( toStrict         )
import           Data.Bifunctor                        ( bimap            )
import           Lens.Micro                            ( (^.), (.~), (&)  )
import           Data.Text                             ( Text             )
import           System.Directory                      ( removeFile       )
import           System.IO.Temp                        ( emptyTempFile    )
import           Control.Monad.Trans                   ( liftIO           )
import           Control.Monad.Except                  ( ExceptT (..)
                                                       , throwError
                                                       , catchError
                                                       , liftEither       )
import           Control.Exception                     ( SomeException
                                                       , catch            )
import           View.View                             ( refToBibtex      )
import           Model.Parsers.BibTex                  ( parseRef         )

-- =============================================================== --
-- Handling external processes with temporary files

cleanUp :: FilePath -> FilePath -> String -> T.ErrMonad a
-- ^Handle exceptions when attempting to envoke an external process p
-- on a temporary file. The exception is rethrown whith a more
-- informative message and the temporary file is removed.
cleanUp p temp errMsg = liftIO (removeFile temp) >> throwError msg
    where msg = unlines [ "Unable to run process '" ++ p ++ "'."
                        , "Aborting with no changes made."
                        , "Additional information: " ++ errMsg ]

runExternal :: String -> T.Ref -> T.ErrMonad T.Ref
-- ^If an entry is not missing, convert it to a formatted BibTeX
-- string and write to a temporary file. Then run the external
-- process p on the file to get a new BibTeX entry, which is parsed
-- in as new a new, modified btx entry and returned. Do not return
-- until either parsing succeeds or the user aborts.
runExternal _ (T.Missing fp k e)  = pure $ T.Missing fp k e
runExternal p r@(T.Ref fp k v   ) = do
    let bibtexString = Tx.append editIntro . refToBibtex k $ v
    temp   <- liftIO . emptyTempFile "." . Tx.unpack $ k <> ".bib"
    result <- catchError ( parseLoop p fp temp bibtexString ) ( cleanUp p temp )
    liftIO . removeFile $ temp
    pure . maybe r id $ result

parseLoop :: String -> FilePath -> FilePath -> Text -> T.ErrMonad (Maybe T.Ref)
-- ^Continue calling the external process p on a single-reference
-- BibTeX file until it parses or the user aborts by deleting all the
-- '@' symbols in the file. Error messages are included in the file
-- if it fails to parse.
parseLoop p fp temp xs = do
    E.writeFileExcept temp $ xs
    E.callProcExcept p [temp]
    content <- E.readFileExcept temp
    if wantsToAbort content
       then pure Nothing
       else case parseRef fp content of
                 Left _  -> parseLoop p fp temp ( addErrMsg content )
                 Right r -> pure . Just $ r

editIntro :: Text
-- ^Instruction to user added as a leading comment when editing.
editIntro = Tx.unlines hs
    where hs = [ "\nPlease edit the *single* BibTeX entry below."
               , "Leading comments will be stripped."
               , "Following comments (other than blank lines) will be treated"
               , "as metadata associated with the reference."
               , "\nTO ABORT THE EDIT:"
                 <> " delete all 'at'-symbols (e.g., the entire entry).\n"
               ]

addErrMsg :: Text -> Text
-- ^Delete any leading comments prior to the reference entry and
-- ^replace with an an failure-to-parse error message along with the
-- standard editing instruction string.
addErrMsg = Tx.append (hs <> editIntro) . Tx.dropWhile ( /= '@' )
    where hs = "\nPARSE ERROR: Check the BibTeX syntax and try again.\n"

wantsToAbort :: Text -> Bool
-- ^Determine whether or not the user wants to abort editing.
wantsToAbort = Tx.null . Tx.dropWhile ( /= '@' )

-- =============================================================== --
-- Interfacing with the internet

type WreqResponse = Wreq.Response BSL.ByteString
type DOI          = String
type WebAddress   = String
type Decoder      = BS.ByteString -> Text

getDoi :: Decoder -> DOI -> T.ErrMonad T.Ref
-- ^Attempt to download a BibTeX reference from a doi. Retrieving
-- references with the indicated Accept header and doi address should
-- result in braced rather than quoted reference fields.
getDoi decode doi = do
    let ps = [ "text/bibliography; style=bibtex" ]
        os = Wreq.defaults & Wreq.header "Accept" .~ ps
        ad = "https://doi.org/" ++ doi
    catchError ( tryToConnectWithGet os ad >>= readResponse decode ad )
               ( pure . T.Missing doi "no-key"                           )

tryToConnectWithGet :: Wreq.Options -> WebAddress -> T.ErrMonad WreqResponse
tryToConnectWithGet ops address = ExceptT $ do
    liftIO . catch ( Right <$> Wreq.getWith ops address ) $ hndlErr
    where hndlErr :: SomeException -> IO ( Either T.ErrString WreqResponse )
          hndlErr _ = pure . Left $ "Unable to connect to " ++ address

readResponse :: Decoder -> WebAddress -> WreqResponse -> T.ErrMonad T.Ref
-- ^Helper function for reading the response to a get request.
readResponse decode ad resp
    | code == 200 = liftEither . perr . parseRef ad . decode . toStrict $ body
    | otherwise   = liftEither . Left $ errs
    where code = resp ^. Wreq.responseStatus . Wreq.statusCode
          body = resp ^. Wreq.responseBody
          errs = "Cannot access " ++ ad ++ ", error code: " ++ show code
          perr = bimap ("Cannot parse: " ++) id
