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
import Data.Maybe                               ( catMaybes         )
import Control.Monad                            ( unless            )
import Control.Monad.Except                     ( throwError
                                                , liftEither        )
import Control.Monad.State.Lazy                 ( get
                                                , put
                                                , lift
                                                , liftIO            )
import Core                                     ( deleteRefs
                                                , getRef
                                                , insertRefs
                                                , isPresent         )
import CoreIO                                   ( readOrMakeFile
                                                , readFileExcept
                                                , writeFileExcept   )
import BibTeX.Parser                            ( parseBib          )
import BibTeX.Resources                         ( genericKey
                                                , genKeyNumber
                                                , supported
                                                , templates         )
import Formatting                               ( argInvalidErr
                                                , bibToBibtex
                                                , cmdInvalidErr
                                                , formatHelp
                                                , formatRef
                                                , refToBibtex
                                                , renameErr
                                                , summarize
                                                , summarizeAllEntries
                                                , summarizeContext
                                                , summarizeEntries  )

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
      , T.Command "new"  newCmd  newCmdSHelp  newCmdLHelp
      , T.Command "pull" pullCmd pullCmdSHelp pullCmdLHelp
      , T.Command "take" takeCmd takeCmdSHelp takeCmdLHelp
        -- Context operators
      , T.Command "name" nameCmd nameCmdSHelp nameCmdLHelp
      , T.Command "send" sendCmd sendCmdSHelp sendCmdLHelp
      , T.Command "toss" tossCmd tossCmdSHelp tossCmdLHelp
      , T.Command "view" viewCmd viewCmdSHelp viewCmdLHelp
      ]

-- =============================================================== --
-- State managers

updateIn :: T.Context -> T.BtxStateMonad T.Bibliography
-- ^Save references in context to the in-bibliography and return the
-- updated bibliography.
updateIn rs = do
    btxState <- get
    let oldBib  = T.inBib btxState
        newRefs = insertRefs ( T.refs oldBib ) rs
        newBib  = oldBib { T.refs = newRefs }
    put btxState { T.inBib = newBib }
    return newBib

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
listCmdSHelp = "list : display a summary of the entries "
               ++ "in the working bibliography"

listCmdLHelp :: String
listCmdLHelp = intercalate "\n" hs
    where hs = [ listCmdSHelp ++ "\n"
               , "This command leaves the current context unchanged. It can be"
               , "used to query whether the working bibliography contains a"
               , "specific entry. If no arguments are supplied, then a summary"
               , "of all entries in the bibliography are shown. If arguments"
               , "are supplied, then summary information is provided for those"
               , "entries whose keys initially match the argument."
               , "For example, the command,\n"
               , "    list Cats Dogs\n"
               , "will return summary information for entries with keys such as"
               , "Cats, Cats1964, CatsAndSquirrels, Dogs, Dogs2016, etc., but"
               , "not ChipmunksAndDogs. See also <view> and <info>."
               ]

listCmd :: T.CommandMonad T.Context
listCmd [] rs = do bib <- T.inBib <$> get
                   liftIO . Tx.putStrLn . summarizeAllEntries $ bib
                   return rs
listCmd xs rs = do bib <- T.inBib <$> get
                   let go = Tx.putStrLn . summarizeEntries bib . Tx.pack
                   liftIO . mapM_ go $ xs
                   return rs

-- =============================================================== --
-- Context constructors

-- getCmd -----------------------------------------------------------

getCmdSHelp :: String
getCmdSHelp = "get [KEY .. ] : copy entries from "
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
    return . map (getRef bib) $ xs

-- newCmd -----------------------------------------------------------

newCmdSHelp :: String
newCmdSHelp = "new [type .. ] : populate context with template entries "
              ++ "of specified types."

newCmdLHelp :: String
newCmdLHelp = intercalate "\n" hs
    where sp = map ( \ x -> replicate 9 ' ' <> fst x ) supported
          hs = [ newCmdSHelp ++ "\n"
               , "The command has the following effects:\n"
               , "  1. Update the working bibliography with the current context"
               , "     and then clear the context."
               , "  2. Add new entry templates to the context with the"
               , "     specified types."
               , "  3. Each entry will be given the key '"
                 ++ Tx.unpack genericKey ++ "', where n is"
               , "     a number, so as to not conflict with any other key in"
               , "     the working bibliography."
               , "  4. The currently supported entry templates are:\n"
               , intercalate "\n" sp ++ "\n"
               , "  5. If an entry is not supported, then a 'blank' type entry"
               , "     is generated with a minimum number of fields."
               ]

newCmd :: T.CommandMonad T.Context
newCmd xs rs = do
    updateIn rs
    bib <- T.inBib <$> get
    let n = genKeyNumber bib
    return . templates n $ xs

-- pullCmd ----------------------------------------------------------

pullCmdSHelp :: String
pullCmdSHelp = "pull [KEY .. ] : move entries from "
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
    put btxState { T.inBib = bib { T.refs = deleteRefs ( T.refs bib ) rs' } }
    return rs'

-- takeCmd ----------------------------------------------------------

takeCmdSHelp :: String
takeCmdSHelp = "take [KEY .. ] : copy entries from "
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
         Just bib -> return . map ( getRef bib ) $ xs

-- =============================================================== --
-- Context operators

-- nameCmd ----------------------------------------------------------

nameCmdSHelp :: String
nameCmdSHelp = "name [KEY .. ] : change key names for entries in the context."

nameCmdLHelp :: String
nameCmdLHelp = intercalate "\n" hs
    where hs = [ nameCmdSHelp ++ "\n"
               , "For example, if you have the following bibliography entries"
               , "in context:"
               , "    Dogs1964"
               , "    Cats1981\n"
               , "then you can rename them as,"
               , "    Dogs1964 --> Squirrels1964"
               , "    Cats1981 --> Monkeys1981\n"
               , "using the command:"
               , "    name Squirrels1964 Monkeys1981\n"
               , "There must be a one-to-one matching between the number of"
               , "names and entries in the context and none of the entries can"
               , "can be missing. If there is not a one-to-one matching, then"
               , "the context remains unchanged.\n"
               , "NOTE: This command only applies in-context, so it should be"
               , "used with <pull>, <send> or <take> commands to change the"
               , "name of the entry in a bibliography. If you use it with"
               , "<get>, then you will end up having a duplicate entry in the"
               , "working bibliography with two different keys."
               ]

nameCmd :: T.CommandMonad T.Context
nameCmd ns rs
    | nn == nr  = return . catMaybes . zipWith go ns $ rs
    | otherwise = ( liftIO . putStrLn $ renameErr nn nr ) >> return rs
    where go n (T.Ref fp k v ) = Just $ T.Ref (newFp fp k) (Tx.pack n) v
          go _ (T.Missing _ _) = Nothing
          newFp fp k           = fp ++ " (originally " ++ Tx.unpack k ++ ")"
          nn                   = length ns
          nr                   = length . filter isPresent $ rs

-- sendCmd ----------------------------------------------------------

sendCmdSHelp :: String
sendCmdSHelp = "send : update export bibliography with the current context."

sendCmdLHelp :: String
sendCmdLHelp = intercalate "\n" hs
    where hs = [ sendCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Update the export bibliography with the current context"
               , "     overwritting any references that have the same keys."
               , "  2. Depopulate the context."
               , "  3. If no export bibliography is set, then nothing happens"
               , "     besides depopulation of the context.\n"
               , "This command is used with the <to> command, which sets the"
               , "export bibliography (see help for <to>). However, there is"
               , "syntactic sugar that combines the two into the command"
               , "<send to>, which takes a file path. For example, the"
               , "following are equivalent:\n"
               , "    get myRef, to myExport.bib, send"
               , "    get myRef and send to myExport.bib"
               ]

sendCmd :: T.CommandMonad T.Context
sendCmd ("to":xs) rs = toCmd xs rs >>= sendCmd []
sendCmd _ rs = do
    btxState <- get
    case T.toBib btxState of
         Nothing     -> return []
         Just oldBib -> do let newRefs = insertRefs ( T.refs oldBib ) rs
                               newBib  = oldBib { T.refs = newRefs }
                           put btxState { T.toBib = Just newBib }
                           return []

-- tossCmd ----------------------------------------------------------

tossCmdSHelp :: String
tossCmdSHelp = "toss : depopulate the context."

tossCmdLHelp :: String
tossCmdLHelp = intercalate "\n" hs
    where hs = [ tossCmdSHelp ++ "\n"
               , "That's all this command does. It is particularly useful for"
               , "deleting entries from the working bibilography using <pull>."
               , "For example, to delete the reference 'myRef' from the"
               , "working bibliography you could use:\n"
               , "    pull myRef and toss"
               ]

tossCmd :: T.CommandMonad [T.Ref]
tossCmd _ rs = return []

-- viewCmd ----------------------------------------------------------

viewCmdSHelp :: String
viewCmdSHelp = "view : view the details of all entries in the context."

viewCmdLHelp :: String
viewCmdLHelp = intercalate "\n" hs
    where hs = [ viewCmdSHelp ++ "\n"
               , "This command has no other effect besides displaying the"
               , "entires in the context in a nicely formatted way. See also"
               , "the <list> and <info> commands."
               ]

viewCmd :: T.CommandMonad [T.Ref]
viewCmd _ [] = do
    liftIO . putStrLn $ "\nNo entries to view.\n"
    return []
viewCmd _ rs = do
    liftIO . Tx.putStrLn $ Tx.empty
    liftIO . Tx.putStrLn . Tx.intercalate "\n\n" . map formatRef $ rs
    return rs

-- =============================================================== --
-- Errors and help

runHelp :: [String] -> String
-- ^Generate a help message
runHelp [] = formatHelp . map T.cmdSHelp $ hub
-- intercalate "\n" . map T.cmdSHelp $ hub
runHelp xs = intercalate "\n" . map ( T.cmdLHelp . route ) $ xs

errCmd :: String -> T.CommandMonad [T.Ref]
errCmd c _ _ = throwError . cmdInvalidErr $ c
