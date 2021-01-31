{-# LANGUAGE LambdaCase #-}

module Main where

-- =============================================================== --
-- Entry and exit point
-- =============================================================== --

import qualified Data.Text.IO          as Tx
import           System.Environment          ( getArgs      )
import           Control.Monad.Except        ( runExceptT   )
import           Controller.Controller       ( configureBtx
                                             , runBtx       )

main :: IO ()
main = do
    args <- getArgs
    runExceptT ( configureBtx args >>= runBtx ) >>= \case
           Left  err -> putStrLn err
           Right msg -> Tx.putStrLn msg
