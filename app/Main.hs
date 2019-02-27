module Main where

-- =============================================================== --
-- Entry and exit point
-- =============================================================== --

import qualified Model.Core.Types as T
import System.Environment               ( getArgs    )
import Control.Monad.Except             ( runExceptT )
import Model.Core.ScriptParser          ( parse      )
import Controller                       ( finish
                                        , getScript
                                        , initBtx
                                        , runBtx
                                        , runHelp    )

main :: IO ()
main = do
    script <- getScript =<< getArgs
    case parse script of
         T.Usage msg      -> finish . Left $ msg
         T.Help cs        -> finish . Left . runHelp $ cs
         T.Script mbFp cs -> runExceptT (initBtx mbFp >>= runBtx cs) >>= finish
