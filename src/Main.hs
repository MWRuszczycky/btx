{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment               ( getArgs           )
import Control.Monad.Except             ( runExceptT        )
import Types                            ( BtxState (..)
                                        , Start    (..)     )
import Commands                         ( runHelp           )
import Core                             ( initBtx
                                        , parseCmds
                                        , runBtx            )

---------------------------------------------------------------------
-- Main

main :: IO ()
-- ^Entry point.
main = do
    start <- parseCmds . unwords <$> getArgs
    case start of
         Usage msg   -> finish . Left $ msg
         Help xs     -> finish . Left . runHelp $ xs
         Normal fp s -> runExceptT ( initBtx s fp >>= runBtx )
                        >>= finish

finish :: Either String BtxState -> IO ()
finish (Left msg) = putStrLn msg
finish (Right _ ) = putStrLn "\nDone."
