{-# LANGUAGE OverloadedStrings #-}

module CoreIO
    ( safeReadFile
    , safeWriteFile
    ) where

import qualified Types        as T
import qualified Data.Text.IO as Tx
import qualified Data.Text    as Tx
import Data.Text            ( Text )
import Control.Exception    ( IOException, catch )
import Control.Monad.Except ( liftEither )
import Control.Monad.Trans  ( liftIO )

-- =============================================================== --
-- IO with exception handling

safeReadFile :: FilePath -> T.ErrMonad Text
safeReadFile fp = do
    content <- liftIO . catch ( Right <$> Tx.readFile fp ) $ hndlErr
    liftEither content
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr e = return . Left $ "Cannot read file '" ++ fp ++ "'!"

safeWriteFile :: FilePath -> Text -> T.ErrMonad ()
safeWriteFile fp x = do
    content <- liftIO . catch ( Right <$> Tx.writeFile fp x ) $ hndlErr
    liftEither content
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr e = return . Left $ "Cannot write file '" ++ fp ++ "'!"
