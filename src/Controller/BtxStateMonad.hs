{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Controller.BtxStateMonad
    ( bibToFile
    , updateIn
    , updateTo
    ) where

-- =============================================================== --
-- IO DSL for working with modeled bibliography information
-- =============================================================== --

import qualified Data.Map.Strict          as Map
import qualified View.Help                as H
import qualified Data.Text                as Tx
import qualified Model.Types              as T
import           Model.BtxState                  ( insertRefs         )
import           Control.Monad.State.Lazy        ( modify, gets, lift )
import           Control.Monad.Except            ( throwError         )
import           Controller.ErrMonad             ( writeFileExcept
                                                 , requestNewKey      )
import           View.View                       ( bibToBibtex        )

bibToFile :: T.Bibliography -> T.BtxStateMonad ()
-- ^Convert a bibliography to BibTeX and write to file.
bibToFile b = lift . writeFileExcept (T.path b) . bibToBibtex $ b

updateIn :: T.Context -> T.BtxStateMonad T.Bibliography
-- ^Save references in context to the in-bibliography and return the
-- updated bibliography.
updateIn rs0 = do
    bib <- gets T.inBib
    rs1 <- mapM ( resolveConflict . T.refs $ bib ) rs0
    modify $ \ s -> s { T.inBib = insertRefs bib rs1 }
    gets T.inBib

updateTo :: T.Context -> T.BtxStateMonad T.Bibliography
-- ^Save references in context to the to-bibliography and return the
-- updated bibligraphy. An error is generated if there is no
-- to-bibliography to export to.
updateTo rs0 = do
    gets T.toBib >>= \case
        Nothing  -> throwError H.missingToBibErr
        Just bib -> do rs1 <- mapM ( resolveConflict . T.refs $ bib ) rs0
                       modify $ \ s -> s { T.toBib = Just $ insertRefs bib rs1 }
                       pure bib

resolveConflict :: T.References -> T.Ref -> T.BtxStateMonad T.Ref
resolveConflict _ r@(T.Missing _ _ _) = pure r
resolveConflict refs r@(T.Ref   fp k1 e)
    | Map.member k1 refs = do k2 <-lift . requestNewKey k1 $ altKey
                              pure (T.Ref fp k2 e)
    | otherwise          = pure r
    where altKey = head . filter ( not . flip Map.member refs ) $ ks
          ks     = map ( \ x -> k1 <> "-" <> (Tx.pack . show) x ) [1..]
