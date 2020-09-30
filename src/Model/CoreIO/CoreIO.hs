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
import Model.Core.Core                  ( insertRefs         )
import Control.Monad.State.Lazy         ( modify, gets, lift )
import Model.CoreIO.ErrMonad            ( writeFileExcept    )
import Model.Core.Formatting            ( bibToBibtex        )

bibToFile :: T.Bibliography -> T.BtxStateMonad ()
-- ^Convert a bibliography to BibTeX and write to file.
bibToFile b = lift . writeFileExcept (T.path b) . bibToBibtex $ b

updateIn :: T.Context -> T.BtxStateMonad T.Bibliography
-- ^Save references in context to the in-bibliography and return the
-- updated bibliography.
updateIn rs = do
    modify $ \ s -> s { T.inBib = addContext rs . T.inBib $ s }
    gets T.inBib

updateTo :: T.Context -> T.BtxStateMonad ( Maybe T.Bibliography )
-- ^Save references in context to the to-bibliography and return the
-- updated bibligraphy.
updateTo rs = do
    modify $ \ s -> s { T.toBib = addContext rs <$> T.toBib s }
    gets T.toBib

addContext :: T.Context -> T.Bibliography -> T.Bibliography
-- ^Add references in a context to a bibliography.
addContext rs bib = bib { T.refs = insertRefs (T.refs bib) rs }
