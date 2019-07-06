module Main where

-- =============================================================== --
-- Entry and exit point
-- =============================================================== --

import qualified Model.Core.Types as T
import qualified Data.Text.IO     as Tx
import System.Environment               ( getArgs       )
import Control.Monad.Except             ( runExceptT    )
import Model.Core.ScriptParser          ( parse         )
import Controller                       ( finish
                                        , getInput
                                        , getStyleMap
                                        , initBtx
                                        , runBtx
                                        , getHelp       )

main :: IO ()
main = do
    input  <- getInput =<< getArgs
    styles <- getStyleMap
    case either T.Usage parse input of
         T.Usage msg      -> finish . Left $ msg
         T.Help cs        -> Tx.putStr $ getHelp styles $ cs
         T.Script mbFp cs -> let startUp = initBtx styles mbFp
                             in  runExceptT (startUp >>= runBtx cs) >>= finish
