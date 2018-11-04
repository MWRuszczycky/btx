{-# LANGUAGE OverloadedStrings #-}

module CoreIO
    ( readOrMakeFile
    , readFileExcept
    , writeFileExcept
    , runExternal
    ) where

import qualified Types        as T
import qualified Data.Text.IO as Tx
import qualified Data.Text    as Tx
import Data.Text                    ( Text                )
import System.Process               ( callProcess         )
import System.Directory             ( removeFile          )
import System.IO.Temp               ( emptyTempFile       )
import System.IO.Error              ( isDoesNotExistError )
import Formatting                   ( refToBibtex         )
import BibTeX.Parser                ( parseRef            )
import Control.Monad.Except         ( ExceptT (..)
                                    , liftEither          )
import Control.Monad.Trans          ( liftIO              )
import Control.Exception            ( try, IOException
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
runExternal p (T.Missing fp k ) = return $ T.Missing fp k
runExternal p (T.Ref fp k v   ) = do
    temp <- liftIO . emptyTempFile "." . Tx.unpack $ k
    writeFileExcept temp ( refToBibtex k v )
    callProcExcept p $ [temp]
    content <- readFileExcept temp
    liftIO . removeFile $ temp
    liftEither . parseRef fp $ content
