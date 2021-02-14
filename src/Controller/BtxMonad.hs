{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.BtxMonad
    ( logView
    , bibToFile
    , updateIn
    , updateTo
    ) where

-- =============================================================== --
-- IO DSL for working with modeled bibliography information
-- =============================================================== --

import qualified Model.BtxState           as B
import qualified Data.Map.Strict          as Map
import qualified View.Help                as H
import qualified Model.Core.Types         as T
import qualified Data.Text                as Tx
import qualified View.View                as V
import qualified View.Core                as Vc
import           Control.Monad.State.Lazy        ( modify, gets, lift )
import           Control.Monad.Except            ( throwError         )
import           Model.Core.ErrMonad             ( writeFileExcept
                                                 , requestNewKey      )

logView :: T.ViewMonad () -> T.BtxMonad ()
-- ^Log text generated using the ViewMonad to the output log.
logView v = Vc.view v >>= modify . B.addToLog

bibToFile :: T.Bibliography -> T.BtxMonad ()
-- ^Convert a bibliography to BibTeX and write to file.
bibToFile b = lift . writeFileExcept (T.path b) . V.bibToBibtex $ b

updateIn :: T.Context -> T.BtxMonad T.Bibliography
-- ^Save references in context to the in-bibliography and return the
-- updated bibliography.
updateIn rs0 = do
    bib <- gets T.inBib
    rs1 <- mapM ( resolveConflict . T.refs $ bib ) rs0
    modify $ \ s -> s { T.inBib = B.insertRefs bib rs1 }
    gets T.inBib

updateTo :: T.Context -> T.BtxMonad T.Bibliography
-- ^Save references in context to the to-bibliography and return the
-- updated bibligraphy. An error is generated if there is no
-- to-bibliography to export to.
updateTo rs0 = do
    gets T.toBib >>= \case
        Nothing  -> throwError H.missingToBibErr
        Just bib -> do rs1 <- mapM ( resolveConflict . T.refs $ bib ) rs0
                       modify $ \ s -> s { T.toBib = Just $ B.insertRefs bib rs1 }
                       pure bib

resolveConflict :: T.References -> T.Ref -> T.BtxMonad T.Ref
resolveConflict _ r@(T.Missing _ _ _) = pure r
resolveConflict refs r@(T.Ref   fp k1 e)
    | Map.member k1 refs = do k2 <-lift . requestNewKey k1 $ altKey
                              pure (T.Ref fp k2 e)
    | otherwise          = pure r
    where altKey = head . filter ( not . flip Map.member refs ) $ ks
          ks     = map ( \ x -> k1 <> "-" <> (Tx.pack . show) x ) [1..]
