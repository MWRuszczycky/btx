{-# LANGUAGE OverloadedStrings #-}

module CoreIO
    ( readOrMakeFile
    , readFileExcept
    , writeFileExcept
    ) where

import qualified Types        as T
import qualified Data.Text.IO as Tx
import qualified Data.Text    as Tx
import System.IO.Error      ( isDoesNotExistError )
import Data.Text            ( Text )
import Control.Exception    ( IOException, catch )
import Control.Monad.Except ( liftEither )
import Control.Monad.Trans  ( liftIO )

-- =============================================================== --
-- IO with exception handling

readOrMakeFile :: FilePath -> T.ErrMonad Text
-- ^Try to read a file, and if it does not exist, then create it.
readOrMakeFile fp = do
    content <- liftIO . catch ( Right <$> Tx.readFile fp ) $ hndlErr
    liftEither content
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr e | isDoesNotExistError e = return . Right $ Tx.empty
                    | otherwise             = return . Left . show $ e

readFileExcept :: FilePath -> T.ErrMonad Text
readFileExcept fp = do
    content <- liftIO . catch ( Right <$> Tx.readFile fp ) $ hndlErr
    liftEither content
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr = return . Left . show

writeFileExcept :: FilePath -> Text -> T.ErrMonad ()
writeFileExcept fp x = do
    content <- liftIO . catch ( Right <$> Tx.writeFile fp x ) $ hndlErr
    liftEither content
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = return . Left . show
