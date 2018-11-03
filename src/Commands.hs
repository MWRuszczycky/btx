{-# LANGUAGE OverloadedStrings #-}

module Commands
    ( route
    , runHelp
    , updateIn
    , done
    , save
    ) where

import qualified Data.Text.IO           as Tx
import qualified Data.Text              as Tx
import qualified Data.Map.Strict        as Map
import qualified Types                  as T
import Data.Text                                ( Text              )
import Data.List                                ( foldl'
                                                , intercalate       )
import Data.Maybe                               ( mapMaybe          )
import Control.Monad                            ( unless            )
import Control.Monad.Except                     ( throwError
                                                , liftEither        )
import Control.Monad.State.Lazy                 ( get
                                                , put
                                                , lift
                                                , liftIO            )
import CoreIO                                   ( readOrMakeFile
                                                , readFileExcept
                                                , writeFileExcept   )
import BibTeX.Parser                            ( parseBib          )
import Formatting                               ( refToBibtex
                                                , formatRef
                                                , bibToBibtex
                                                , summarize
                                                , summarizeContext
                                                , argInvalidErr
                                                , cmdInvalidErr
                                                , renameErr         )

-- =============================================================== --
-- Hub and router

route :: String -> T.Command T.Context
route c = go hub
    where go []     = T.Command "err" (errCmd c) [] []
          go (x:xs) | T.cmdName x == c = x
                    | otherwise        = go xs

hub :: [ T.Command T.Context ]
hub = [ -- Bibliography managers
        T.Command "in"   inCmd   inCmdSHelp   inCmdLHelp
      , T.Command "to"   toCmd   toCmdSHelp   toCmdLHelp
      , T.Command "from" fromCmd fromCmdSHelp fromCmdLHelp
        -- Queries
      , T.Command "info" infoCmd infoCmdSHelp infoCmdLHelp
      , T.Command "list" listCmd listCmdSHelp listCmdLHelp
        -- Context constructors
      , T.Command "get"  getCmd  getCmdSHelp  getCmdLHelp
      , T.Command "pull" pullCmd pullCmdSHelp pullCmdLHelp
      , T.Command "take" takeCmd takeCmdSHelp takeCmdLHelp
        -- Context operators
      , T.Command "name" nameCmd "short help" "name help"
      , T.Command "send" sendCmd "short help" "send help"
      , T.Command "toss" tossCmd "short help" "toss help"
      , T.Command "view" viewCmd "short help" "view help"
      ]

-- =============================================================== --
-- Utilities

updateIn :: T.Context -> T.BtxStateMonad T.Bibliography
-- ^Save references in context to the in-bibliography and return the
-- updated bibliography.
updateIn rs = do
    btxState <- get
    let oldBib  = T.inBib btxState
        newRefs = insert ( T.refs oldBib ) rs
        newBib  = oldBib { T.refs = newRefs }
    put btxState { T.inBib = newBib }
    return newBib

insert :: T.References -> T.Context -> T.References
-- ^Update a reference map with a list of references.
insert = foldl' ( flip $ uncurry Map.insert )

delete :: T.References -> T.Context -> T.References
delete refs = foldl' ( flip Map.delete ) refs . fst . unzip

getRef :: T.Bibliography -> String -> Maybe T.Ref
-- ^Lookup a reference from a bibliography and package as a Ref.
getRef bib x = (,) key <$> Map.lookup key ( T.refs bib )
    where key = Tx.pack x

save :: T.Bibliography -> T.BtxStateMonad ()
-- ^Convert a bibliography to BibTeX and write to memory.
save b = lift . writeFileExcept (T.path b) . bibToBibtex $ b

done :: T.Context -> T.BtxStateMonad ()
-- ^Save the current context references to the in-bibliography and
-- write both the in- and to-bibliographies.
done rs = do
    updateIn rs >>= save
    btxState <- get
    maybe ( return () ) save . T.toBib $ btxState

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Commands
---------------------------------------------------------------------
---------------------------------------------------------------------

-- =============================================================== --
-- Bibliography constructors, operators and utilities

-- inCmd ------------------------------------------------------------

inCmdSHelp :: String
inCmdSHelp = "in FILE-PATH : reset or create new working bibliography"

inCmdLHelp :: String
inCmdLHelp = intercalate "\n" hs
    where hs = [ inCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Update working bibliography with the current context."
               , "  2. Save the updated working bibliography to disk."
               , "  3. Clear the current context."
               , "  4. Load the .bib file at FILE-PATH as the new working"
               , "     bibliography. If the file does not already exist, then"
               , "     it is created."
               ]

inCmd :: T.CommandMonad T.Context
inCmd xs rs
    | null xs       = throwError "Command <in> requires a file path."
    | length xs > 1 = throwError "Command <in> allows only one argument."
    | otherwise     = do updateIn rs >>= save
                         btxState <- get
                         let fp = head xs
                         content <- lift . readOrMakeFile $ fp
                         bib <- liftEither . parseBib fp $ content
                         put btxState { T.inBib = bib }
                         return []

-- toCmd ------------------------------------------------------------

toCmdSHelp :: String
toCmdSHelp = "to FILE-PATH : reset or create new export bibliography"

toCmdLHelp :: String
toCmdLHelp = intercalate "\n" hs
    where hs = [ inCmdSHelp ++ "\n"
               , "The export bibliography is separate from the working"
               , "bibliography (set with <in>) and represents a target where"
               , "references can be exported using the <send> command. This"
               , "command has the following effects:\n"
               , "  1. Leave the current context unchanged."
               , "  2. Save the current export bibliography if it exists."
               , "  3. Load the .bib file at FILE-PATH as the new export"
               , "     bibliography. If the file does not exist, then it is"
               , "     created. If the file path is the same as that for the"
               , "     working bibliography, then the export bibliography"
               , "     is unset and nothing else happens.\n"
               , "See also help for the <send> command."
               ]

toCmd :: T.CommandMonad T.Context
toCmd xs rs
    | null xs       = throwError "Command <to> requires a file path."
    | length xs > 1 = throwError "Command <to> allows only one argument."
    | otherwise     = do let fp = head xs
                         btxState <- get
                         maybe ( return () ) save $ T.toBib btxState
                         if fp == ( T.path . T.inBib ) btxState
                            then put btxState { T.toBib = Nothing }
                            else do content <- lift . readOrMakeFile $ fp
                                    bib <- liftEither . parseBib fp $ content
                                    put btxState { T.toBib = Just bib }
                         return rs

-- fromCmd ----------------------------------------------------------

fromCmdSHelp :: String
fromCmdSHelp = "from FILE-PATH : reset the import bibliography."

fromCmdLHelp :: String
fromCmdLHelp = intercalate "\n" hs
    where hs = [ fromCmdSHelp ++ "\n"
               , "The import bibliography is a bibliography separate from the"
               , "working bibliography (set with <in>) that you can use to"
               , "populate the context with references using the <take>"
               , "command. This is useful for building new bibliographies from"
               , "previously existing ones. The import bibliography is never"
               , "modified. The <from> command has the following effects:\n"
               , "  1. If FILE-PATH does not exist, then an error is generated."
               , "  2. If FILE-PATH is the same as the working bibliography,"
               , "     then the import bibliography becomes unset."
               , "  3. The context is left unchanged.\n"
               , "See also help for the <take> command."
               ]

fromCmd :: T.CommandMonad T.Context
fromCmd xs rs
    | null xs       = throwError "Commad <from> requires a file path."
    | length xs > 1 = throwError "Command <from> allows only one argument."
    | otherwise     = do btxState <- get
                         let fp = head xs
                         if fp == ( T.path . T.inBib ) btxState
                            then put btxState { T.fromBib = Nothing }
                            else do content <- lift . readFileExcept $ fp
                                    bib <- liftEither . parseBib fp $ content
                                    put btxState { T.fromBib = Just bib }
                         return rs

-- =============================================================== --
-- Queries

-- infoCmd ----------------------------------------------------------

infoCmdSHelp :: String
infoCmdSHelp = "info : display summary of all bibliographies "
               ++ "and the current context."

infoCmdLHelp :: String
infoCmdLHelp = intercalate "\n" hs
    where hs = [ infoCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Leave the current context unchanged."
               , "  2. Display what is currently in context."
               , "  3. List the working, import and export bibliographies with"
               , "     summary information.\n"
               , "See also <list> and <view>."
               ]

infoCmd :: T.CommandMonad T.Context
infoCmd _ rs = get >>= liftIO . Tx.putStrLn . flip summarize rs >> return rs

---------------------------------------------------------------------

listCmdSHelp :: String
listCmdSHelp = "list : display a summary of the current context "

listCmdLHelp :: String
listCmdLHelp = intercalate "\n" hs
    where hs = [ listCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Leave the current context unchanged."
               , "  2. Summarize what is currently in context.\n"
               , "See also <view> and <info>."
               ]

listCmd :: T.CommandMonad T.Context
listCmd _ rs = ( liftIO . Tx.putStrLn . summarizeContext $ rs ) >> return rs

-- =============================================================== --
-- Context constructors

-- getCmd -----------------------------------------------------------

getCmdSHelp :: String
getCmdSHelp = "get [KEY ..] : copy entries from "
              ++ "working bibliography to context."

getCmdLHelp :: String
getCmdLHelp = intercalate "\n" hs
    where hs = [ getCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Update the working bibliography with the current context"
               , "     and then clear the context."
               , "  2. Repopulate the context with the entries in the working"
               , "     bibliography having the specified KEYs without further"
               , "     changing the working bibliography."
               , "  3. If a KEY is not found in the working bibliography, then"
               , "     no corresponding reference is copied to the context.\n"
               , "See also <pull>, which deletes the entries from the working"
               , "bibliography after moving them to the context, and <take>."
               ]

getCmd :: T.CommandMonad T.Context
getCmd xs rs = do
    updateIn rs
    bib <- T.inBib <$> get
    return . mapMaybe ( getRef bib ) $ xs

-- pullCmd ----------------------------------------------------------

pullCmdSHelp :: String
pullCmdSHelp = "pull [KEY ..] : move entries from "
              ++ "working bibliography to context."

pullCmdLHelp :: String
pullCmdLHelp = intercalate "\n" hs
    where hs = [ pullCmdSHelp ++ "\n"
               , "This command is the same as <get>; however the entries copied"
               , "from the working bibliography are then deleted from the"
               , "working bibliography. Therefore, this command can be used to"
               , "remove entries from a bibliography using the script:\n"
               , "    pull [KEY ..] and toss\n"
               , "See also <get> and <toss>."
               ]

pullCmd :: T.CommandMonad T.Context
pullCmd xs rs = do
    rs' <- getCmd xs rs
    btxState <- get
    let bib = T.inBib btxState
    put btxState { T.inBib = bib { T.refs = delete ( T.refs bib ) rs' } }
    return rs'

-- takeCmd ----------------------------------------------------------

takeCmdSHelp :: String
takeCmdSHelp = "take [KEY ..] : copy entries from "
               ++ "import bibliography to context"

takeCmdLHelp :: String
takeCmdLHelp = intercalate "\n" hs
    where hs = [ takeCmdSHelp ++ "\n"
               , "This command is the same as <get>; however the entries copied"
               , "from the import bibliography rather than the working."
               , "bibliography. If the import bibliography is unset, then"
               , "the context is just cleared."
               ]

takeCmd :: T.CommandMonad T.Context
takeCmd xs rs = do
    updateIn rs
    btxState <- get
    case T.fromBib btxState of
         Nothing  -> return []
         Just bib -> return . mapMaybe ( getRef bib ) $ xs

-- =============================================================== --
-- Context operators

---------------------------------------------------------------------

sendCmd :: T.CommandMonad [T.Ref]
-- ^Update the to-bibliography with the context and depopulate it.
-- If there is no to-bibliography, then just depopulate the context.
sendCmd ("to":xs) rs = toCmd xs rs >>= sendCmd []
sendCmd _ rs = do
    btxState <- get
    case T.toBib btxState of
         Nothing     -> return []
         Just oldBib -> do let newRefs = insert ( T.refs oldBib ) rs
                               newBib  = oldBib { T.refs = newRefs }
                           put btxState { T.toBib = Just newBib }
                           return []

---------------------------------------------------------------------

tossCmd :: T.CommandMonad [T.Ref]
tossCmd _ rs = return []

---------------------------------------------------------------------

viewCmd :: T.CommandMonad [T.Ref]
viewCmd _ [] = do
    liftIO . putStrLn $ "\nNo entries to view.\n"
    return []
viewCmd _ rs = do
    liftIO . Tx.putStrLn $ Tx.empty
    liftIO . Tx.putStrLn . Tx.intercalate "\n\n" . map formatRef $ rs
    return rs

---------------------------------------------------------------------

nameCmd :: T.CommandMonad [T.Ref]
nameCmd ns rs
    | nn == nr  = return . zipWith ( \ n (k,v) -> (Tx.pack n, v) ) ns $ rs
    | otherwise = ( liftIO . putStrLn $ renameErr nn nr ) >> return rs
    where nn = length ns
          nr = length rs

-- =============================================================== --
-- Errors and help

---------------------------------------------------------------------

runHelp :: [String] -> String
-- ^Generate a help message
runHelp [] = intercalate "\n" . map T.cmdSHelp $ hub
runHelp xs = intercalate "\n" . map ( T.cmdLHelp . route ) $ xs

errCmd :: String -> T.CommandMonad [T.Ref]
errCmd c _ _ = throwError . cmdInvalidErr $ c
