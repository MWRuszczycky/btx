{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment               ( getArgs           )
import Control.Monad.Except             ( runExceptT        )
import Core                             ( initBtx
                                        , parseCmds
                                        , runBtx            )

---------------------------------------------------------------------
-- Main

main :: IO ()
-- ^Entry point.
main = do
    script <- parseCmds . unwords <$> getArgs
    result <- runExceptT $ runBtx =<< initBtx script
    case result of
         Left msg  -> putStrLn msg
         otherwise -> putStrLn "\nDone."
