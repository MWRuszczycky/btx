{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Model.Macros
    ( embedFile
    , gitHash
    ) where

import qualified Language.Haskell.TH as TH

embedFile :: FilePath -> TH.Q TH.Exp
embedFile fp = do
    content <- TH.runIO . readFile $ fp
    [| content |]

gitHash :: TH.Q TH.Exp
gitHash = do
    content <- TH.runIO . readFile $ ".git/HEAD"
    case words content of
         "ref:":path:_ -> do hash <- TH.runIO . readFile $ ".git/" <> path
                             [| hash |]
         _             -> [| "git-hash-missing" |]
