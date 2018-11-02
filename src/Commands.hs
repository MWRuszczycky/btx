{-# LANGUAGE OverloadedStrings #-}

module Commands
    ( compile
    , parseCmds
    , initBib
    , route
    ) where

import qualified Data.Text.IO           as Tx
import qualified Data.Text              as Tx
import qualified Data.Map.Strict        as Map
import qualified Types                  as T
import qualified Resources.Resources    as R
import Data.Text                 ( Text )
import Control.Monad.Except      ( throwError )
import Control.Monad.State.Lazy  ( get
                                 , put
                                 , lift
                                 , liftIO )
import CoreIO                    ( safeReadFile )
import BibTeXParser              ( parseBibtex )
import Formatting                ( refToBibtex
                                 , formatRef
                                 , summarize )

-- =============================================================== --
-- Initialization

-- Exported

initBib :: T.Bibliography -> T.BtxState
initBib b =  T.BtxState { T.inBib   = b
                        , T.toBib   = Nothing
                        , T.fromBib = Nothing
                        }

-- =============================================================== --
-- Parsing and compiling

---------------------------------------------------------------------
-- Compiling

-- Exported

compile :: [ T.Command [T.Ref] ] -> [T.Ref] -> T.BtxStateMonad ()
compile []                          _  = return ()
compile ((T.Command _ m xs _) : cs) rs = m xs rs >>= compile cs

---------------------------------------------------------------------
-- Parsing

-- Exported

parseCmds :: String -> [ T.Command [T.Ref] ]
parseCmds = parseAnd . words . splitAnd

-- Unexported

splitAnd :: String -> String
splitAnd []        = []
splitAnd (',':xs)  = " and " ++ splitAnd xs
splitAnd ('\n':xs) = " and " ++ splitAnd xs
splitAnd (x:xs)    = x : splitAnd xs

parseAnd :: [String] -> [ T.Command [T.Ref] ]
parseAnd []          = []
parseAnd ("and":xs)  = parseAnd xs
parseAnd (x:xs)      = route x ys : parseAnd zs
    where (ys,zs) = break ( == "and" ) xs

-- =============================================================== --
-- Command router

-- Exported

route :: String -> [String] -> T.Command [T.Ref]
route "in"   xs = T.Command "in"   inCmd    xs "in help"
route "view" xs = T.Command "view" viewCmd  xs "view help"
route "get"  xs = T.Command "get"  getCmd   xs "get help"
route "info" xs = T.Command "info" infoCmd  xs "info help"
route c      _  = T.Command "err"  ( errCmd c ) [] []

-- =============================================================== --
-- Commands

viewCmd :: T.CommandMonad [T.Ref]
viewCmd _ [] = do
    liftIO . putStrLn $ "\nNo entries to view.\n"
    return []
viewCmd _ rs = do
    liftIO . Tx.putStrLn $ Tx.empty
    liftIO . Tx.putStrLn . Tx.intercalate "\n\n" . map formatRef $ rs
    return rs

---------------------------------------------------------------------

inCmd :: T.CommandMonad [T.Ref]
-- ^Set the in-bibliography.
inCmd xs _
    | null xs       = throwError "Command <in> requires a file path."
    | length xs > 1 = throwError "Command <in> allows only one argument."
    | otherwise     = do bst <- get
                         let fp = head xs
                         content <- lift . safeReadFile $ fp
                         case parseBibtex fp content of
                              Left err  -> throwError err
                              Right bib -> put bst { T.inBib = bib }
                         return []

---------------------------------------------------------------------

infoCmd :: T.CommandMonad [T.Ref]
infoCmd _ _ = do
    bib <- T.inBib <$> get
    liftIO . Tx.putStrLn . summarize $ bib
    return []

---------------------------------------------------------------------

getCmd :: T.CommandMonad [T.Ref]
-- ^Extract a reference from the in-bibliography.
getCmd ks _ = do
    bib <- T.inBib <$> get
    rs <- liftIO . mapM ( getRef bib ) $ ks
    return . concat $ rs

getRef :: T.Bibliography -> String -> IO [T.Ref]
getRef bib k = let key = Tx.pack k
               in case Map.lookup key ( T.refs bib ) of
                       Just ref -> return [ (key, ref) ]
                       Nothing  -> do putStrLn $ R.notFound k ( T.path bib )
                                      return []

---------------------------------------------------------------------

errCmd :: String -> T.CommandMonad [T.Ref]
errCmd c _ _ = throwError . R.unrecognized $ c
