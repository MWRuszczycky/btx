{-# LANGUAGE OverloadedStrings #-}

module Model.CoreIO.CoreIO
    ( bibToFile
    , updateIn
    , updateTo
    ) where

-- =============================================================== --
-- IO DSL for working with modeled bibliography information
-- =============================================================== --

import qualified Model.Core.Types as T
import Model.Core.Core                  ( insertRefs      )
import Control.Monad.State.Lazy         ( get, put, lift  )
import Model.CoreIO.ErrMonad            ( writeFileExcept )
import Model.Core.Formatting            ( bibToBibtex     )

bibToFile :: T.Bibliography -> T.BtxStateMonad ()
-- ^Convert a bibliography to BibTeX and write to file.
bibToFile b = lift . writeFileExcept (T.path b) . bibToBibtex $ b

updateIn :: T.Context -> T.BtxStateMonad T.Bibliography
-- ^Save references in context to the in-bibliography and return the
-- updated bibliography.
updateIn rs = do
    btxState <- get
    let oldBib  = T.inBib btxState
        newRefs = insertRefs ( T.refs oldBib ) rs
        newBib  = oldBib { T.refs = newRefs }
    put btxState { T.inBib = newBib }
    pure newBib

updateTo :: T.Context -> T.BtxStateMonad ( Maybe T.Bibliography )
-- ^Save references in context to the to-bibliography and return the
-- updated bibligraphy.
updateTo rs = do
    btxState <- get
    let newBib = T.toBib btxState
                 >>= \ b -> pure b { T.refs = insertRefs (T.refs b) rs }
    put btxState { T.toBib = newBib }
    pure newBib
