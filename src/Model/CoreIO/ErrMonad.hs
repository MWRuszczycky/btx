{-# LANGUAGE OverloadedStrings #-}

module Model.CoreIO.ErrMonad
    ( -- File management
      readOrMakeFile
    , readFileExcept
    , writeFileExcept
      -- External processes
    , callProcExcept
    ) where

-- =============================================================== --
-- Wrappers for the ErrMonad type
-- =============================================================== --

import qualified Data.Text.IO        as Tx
import qualified Data.Text           as Tx
import qualified Model.Core.Types    as T
import Data.Text                             ( Text                )
import System.Process                        ( callProcess         )
import System.IO.Error                       ( isDoesNotExistError )
import Control.Monad.Trans                   ( liftIO              )
import Control.Monad.Except                  ( ExceptT (..)        )
import Control.Exception                     ( IOException
                                             , displayException
                                             , catch               )

-- =============================================================== --
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

-- =============================================================== --
-- Working with external processes

callProcExcept :: FilePath -> [String] -> T.ErrMonad ()
-- ^Wraps an external process inside the ErrMonad monad.
callProcExcept p args = ExceptT $ do
    liftIO . catch ( Right <$> callProcess p args ) $ hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = return . Left . displayException
