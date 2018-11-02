{-# LANGUAGE OverloadedStrings #-}

module Resources.Resources
    ( -- Error messages
      notFound
    , unrecognized
    ) where

import qualified Types        as T
import Data.Text            ( Text )

-- =============================================================== --
-- Error messages

notFound :: String -> FilePath -> T.ErrString
notFound k fp = "Entry " ++ k ++ " not found in bibliography " ++ fp

unrecognized :: String -> T.ErrString
unrecognized = (++) "Unrecognized command: "

-- =============================================================== --
-- Help strings

-- Coming soon!

---------------------------------------------------------------------
