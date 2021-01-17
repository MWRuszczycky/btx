{-# LANGUAGE OverloadedStrings #-}

module Controller.ErrMonad
    ( -- File management
      readOrMakeFile
    , readFileExcept
    , writeFileExcept
      -- External processes
    , callProcExcept
      -- CLI interaction with user
    , requestNewKey
    ) where

-- =============================================================== --
-- Wrappers for the ErrMonad type
-- =============================================================== --

import qualified Model.Types          as T
import qualified Data.Text.IO         as Tx
import qualified Data.Text            as Tx
import           Data.Text                  ( Text                 )
import           Data.Char                  ( isSpace              )
import           System.Process             ( callProcess          )
import           System.IO                  ( stdout, hFlush
                                            , stdin, hIsClosed     )
import           System.IO.Error            ( isDoesNotExistError  )
import           Control.Monad.Except       ( ExceptT (..), liftIO )
import           Control.Exception          ( IOException
                                            , displayException
                                            , catch                )

-- =============================================================== --
-- Reading and writing files

readOrMakeFile :: FilePath -> T.ErrMonad Text
-- ^Try to read a file, and if it does not exist, then create it.
readOrMakeFile fp = ExceptT $ do
    catch ( Right <$> Tx.readFile fp ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr e | isDoesNotExistError e = pure . Right $ Tx.empty
                    | otherwise             = pure . Left . show $ e

readFileExcept :: FilePath -> T.ErrMonad Text
readFileExcept fp = ExceptT $ do
    catch ( Right <$> Tx.readFile fp ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr = pure . Left . show

writeFileExcept :: FilePath -> Text -> T.ErrMonad ()
writeFileExcept fp x = ExceptT $ do
    catch ( Right <$> Tx.writeFile fp x ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = pure . Left . show

-- =============================================================== --
-- Working with external processes

callProcExcept :: FilePath -> [String] -> T.ErrMonad ()
-- ^Wraps an external process inside the ErrMonad monad.
callProcExcept p args = ExceptT $ do
    catch ( Right <$> callProcess p args ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = pure . Left . displayException

-- =============================================================== --
-- CLI interaction with the user

requestNewKey :: T.Key -> T.Key -> T.ErrMonad T.Key
-- ^A key for a new entry is already present in the bibliography. We
-- want to determine whether the user wants to overwrite this entry
-- or use a new key. A unique alternative key is suggested.
requestNewKey oldKey uniqueKey = do
    let msg1 = [ "An entry already has key"
               , "'" <> oldKey <> "'"
               , "and will be overwritten.\nEnter 'x' to continue with"
               , "overwrite.\nPress <enter> to change key to:"
               , "'" <> uniqueKey <> "'\nbtx>"
               ]
        msg2 = [ "Key '" <> oldKey <> "' is already in use,"
               , "so unique key '" <> uniqueKey <> "' was used instead."
               ]
    isClosed <- liftIO . hIsClosed $ stdin
    if isClosed
       then do liftIO . Tx.putStrLn . Tx.unwords $ msg2
               pure uniqueKey
       else do liftIO . Tx.putStr . Tx.unwords $ msg1
               liftIO . hFlush $ stdout
               response <- liftIO getLine
               case filter (not . isSpace) response of
                    "x" -> pure oldKey
                    "X" -> pure oldKey
                    _   -> pure uniqueKey
