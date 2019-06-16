module Main where

-- =============================================================== --
-- Entry and exit point
-- =============================================================== --

import qualified Model.Core.Types as T
import System.Environment               ( getArgs       )
import Control.Monad.Except             ( runExceptT    )
import Model.Core.Formatting            ( defaultStyles )
import Model.Core.ScriptParser          ( parse         )
import Controller                       ( finish
                                        , getInput
                                        , initBtx
                                        , runBtx
                                        , runHelp       )

main :: IO ()
main = do
    input <- getInput =<< getArgs
    case either T.Usage parse input of
         T.Usage msg      -> finish . Left $ msg
         T.Help cs        -> finish . Left . runHelp $ cs
         T.Script mbFp cs -> let startUp = initBtx defaultStyles mbFp
                             in  runExceptT (startUp >>= runBtx cs) >>= finish
