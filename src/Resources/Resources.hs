{-# LANGUAGE OverloadedStrings #-}

module Resources.Resources
    ( -- Error messages
      cannotRename
    , notFound
    , unrecognized
    ) where

import qualified Types        as T
import Data.Text                    ( Text        )
import Data.List                    ( intercalate )

-- =============================================================== --
-- Error messages

notFound :: String -> FilePath -> T.ErrString
notFound k fp = "Entry " ++ k ++ " not found in bibliography " ++ fp

unrecognized :: String -> T.ErrString
unrecognized = (++) "Unrecognized command: "

cannotRename :: Int -> Int -> T.ErrString
cannotRename n r = intercalate "\n" es
    where es = [ "The entries cannot be renamed, because the number of"
               , "entries currently in the context (" ++ show r
                  ++ ") does not match"
               , "the number of new names supplied (" ++ show n ++ ")." ]

-- =============================================================== --
-- Help strings

-- Coming soon!

---------------------------------------------------------------------
