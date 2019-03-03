{-# LANGUAGE OverloadedStrings #-}

module Commands
    ( hub
    , route
    ) where

-- =============================================================== --
-- This is where all the commands and their help strings are defined.
-- Commands are documented using their help strings.
-- =============================================================== --

import qualified Data.Text.IO             as Tx
import qualified Data.Text                as Tx
import qualified Data.Map.Strict          as Map
import qualified Model.Core.Types         as T
import qualified Model.Core.Messages.Help as H
import Data.Text                                 ( Text             )
import Data.List                                 ( find
                                                 , foldl'
                                                 , intercalate      )
import Control.Monad                             ( unless           )
import Control.Monad.Except                      ( throwError
                                                 , liftEither       )
import Control.Monad.State.Lazy                  ( get
                                                 , put
                                                 , lift
                                                 , liftIO           )
import Model.Core.Core                           ( addToLog
                                                 , allKeysToArgs
                                                 , deleteRefs
                                                 , dropRefByKey
                                                 , searchRefs
                                                 , getRef
                                                 , insertRefs
                                                 , isPresent        )
import Model.CoreIO.CoreIO                       ( bibToFile
                                                 , updateIn
                                                 , updateTo         )
import Model.CoreIO.External                     ( getDoi
                                                 , runExternal      )
import Model.CoreIO.ErrMonad                     ( readOrMakeFile
                                                 , readFileExcept
                                                 , writeFileExcept  )
import Model.BibTeX.Parser                       ( parseBib         )
import Model.BibTeX.Resources                    ( genericKey
                                                 , genKeyNumber
                                                 , supported
                                                 , templates        )
import Model.Core.Formatting                     ( bibToBibtex
                                                 , viewRef
                                                 , refToBibtex
                                                 , summarize
                                                 , summarizeEntry   )

-- =============================================================== --
-- Hub and router

route :: String -> T.Command T.Context
-- ^Get the requested command function based on the command string.
route x = maybe (errCmd x) id . find ( (== x) . T.cmdName) $ hub

hub :: [ T.Command T.Context ]
-- ^Organizes all the command functions, command strings and both the
-- long and short help strings.
hub = [ -- Bibliography managers
        T.Command "from" fromCmd fromCmdSHelp fromCmdLHelp
      , T.Command "in"   inCmd   inCmdSHelp   inCmdLHelp
      , T.Command "save" saveCmd saveCmdSHelp saveCmdLHelp
      , T.Command "to"   toCmd   toCmdSHelp   toCmdLHelp
        -- Queries
      , T.Command "info" infoCmd infoCmdSHelp infoCmdLHelp
      , T.Command "view" viewCmd viewCmdSHelp viewCmdLHelp
        -- Context constructors
      , T.Command "doi"  doiCmd  doiCmdSHelp  doiCmdLHelp
      , T.Command "find" findCmd findCmdSHelp findCmdLHelp
      , T.Command "get"  getCmd  getCmdSHelp  getCmdLHelp
      , T.Command "new"  newCmd  newCmdSHelp  newCmdLHelp
      , T.Command "pull" pullCmd pullCmdSHelp pullCmdLHelp
      , T.Command "take" takeCmd takeCmdSHelp takeCmdLHelp
        -- Context operators
      , T.Command "edit" editCmd editCmdSHelp editCmdLHelp
      , T.Command "name" nameCmd nameCmdSHelp nameCmdLHelp
      , T.Command "send" sendCmd sendCmdSHelp sendCmdLHelp
      , T.Command "toss" tossCmd tossCmdSHelp tossCmdLHelp
      ]

-- =============================================================== --
-- Bibliography constructors, operators and utilities

-- from command -----------------------------------------------------

fromCmdSHelp :: String
fromCmdSHelp = "from [FILE-PATH] : reset the import bibliography"

fromCmdLHelp :: String
fromCmdLHelp = unlines hs
    where hs = [ fromCmdSHelp ++ "\n"
               , "The import bibliography is a bibliography separate from the"
               , "working bibliography (set with <in>) that you can use to"
               , "populate the context with references using the <take>"
               , "command. This is useful for building new bibliographies from"
               , "previously existing ones. The import bibliography is never"
               , "modified. The <from> command has the following effects:\n"
               , "  1. The context is left unchanged."
               , "  2. If FILE-PATH does not exist, then an error is generated."
               , "  3. If FILE-PATH is the same as the working bibliography,"
               , "     then the import bibliography becomes unset."
               , "  4. If no FILE-PATH is provided, then the import"
               , "     bibliography becomes unset.\n"
               , "See also help for the <take> command."
               ]

fromCmd :: T.CommandMonad T.Context
fromCmd xs rs
    | null xs       = get >>= \ b -> put b { T.fromBib = Nothing } >> pure rs
    | length xs > 1 = throwError "Command <from> allows one or no argument.\n"
    | otherwise     = do btxState <- get
                         let fp = head xs
                         if fp == ( T.path . T.inBib ) btxState
                            then fromCmd [] rs
                            else do content <- lift . readFileExcept $ fp
                                    bib <- liftEither . parseBib fp $ content
                                    put btxState { T.fromBib = Just bib }
                                    pure rs

-- in command -------------------------------------------------------

inCmdSHelp :: String
inCmdSHelp = "in    FILE-PATH : initialize, reset or create the working"
             ++ " bibliography"

inCmdLHelp :: String
inCmdLHelp = unlines hs
    where hs = [ inCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Update working bibliography with the current context."
               , "  2. Save the updated working bibliography to disk."
               , "  3. Clear the current context."
               , "  4. Load the .bib file at FILE-PATH as the new working"
               , "     bibliography. If the file does not already exist, then"
               , "     it is created.\n"
               , "When <in> is used as the first command, then it loads"
               , "FILE-PATH as the initial working bibliography or creates it"
               , "if the file does not already exist. When <in> is not the"
               , "first command, then btx will search the current working"
               , "directory for a unique .bib file, which will be loaded as"
               , "the current working bibliography."
               ]

inCmd :: T.CommandMonad T.Context
inCmd []      rs = throwError "Command <in> requires a file path.\n"
inCmd (_:_:_) rs = throwError "Command <in> allows only one argument.\n"
inCmd (fp:_)  rs = do updateIn rs >>= bibToFile
                      btxState <- get
                      content  <- lift . readOrMakeFile $ fp
                      bib      <- liftEither . parseBib fp $ content
                      put btxState { T.inBib = bib }
                      pure []

-- save command -----------------------------------------------------

saveCmdSHelp :: String
saveCmdSHelp = "save : update working bibliography and write everything to file"

saveCmdLHelp :: String
saveCmdLHelp = unlines hs
    where hs = [ saveCmdSHelp ++ "\n"
               , "This command is only necessary when working interactively"
               , "from standard input. Otherwise it is automatically added to"
               , "the end of any script read from the command line or file."
               , "<save> ignores all arguments and does the following:\n"
               , "  1. Update working bibliography with the current context."
               , "  2. Write the updated working bibliography to file."
               , "  3. Write the export bibliography to file."
               , "  4. Clear the context.\n"
               , "The normal usage of <save> would be at the end of a script"
               , "entered interactively via standard input and before inputting"
               , "ctrl-C to terminate standard input."
               ]

saveCmd :: T.CommandMonad T.Context
saveCmd _ rs = do updateIn rs >>= bibToFile
                  get >>= maybe (pure ()) bibToFile . T.toBib
                  pure []

-- to command -------------------------------------------------------

toCmdSHelp :: String
toCmdSHelp = "to   [FILE-PATH] : reset or create new export bibliography"

toCmdLHelp :: String
toCmdLHelp = unlines hs
    where hs = [ toCmdSHelp ++ "\n"
               , "The export bibliography is separate from the working"
               , "bibliography (set with <in>) and represents a target where"
               , "references can be exported using the <send> command. This"
               , "command has the following effects:\n"
               , "  1. Leave the current context unchanged."
               , "  2. Save the current export bibliography if it exists."
               , "  3. Load the .bib file at FILE-PATH as the new export"
               , "     bibliography. If the file does not exist, then it is"
               , "     created."
               , "  4. If FILE-PATH is the same as that for the working"
               , "     bibliography, then the export bibliography is unset and"
               , "     nothing else happens."
               , "  5. If no file path argument is supplied, then the current"
               , "     export bibliography is saved and unset.\n"
               , "See also help for the <send> command."
               ]

toCmd :: T.CommandMonad T.Context
toCmd (_:_:_) _  = throwError "Command <to> allows only one or no argument.\n"
toCmd xs      rs = do btxState <- get
                      maybe ( pure () ) bibToFile $ T.toBib btxState
                      let fp = head xs
                      if null xs || fp == (T.path . T.inBib) btxState
                         then put btxState { T.toBib = Nothing }
                         else do content <- lift . readOrMakeFile $ fp
                                 bib <- liftEither . parseBib fp $ content
                                 put btxState { T.toBib = Just bib }
                      pure rs

-- =============================================================== --
-- Queries

-- info command -----------------------------------------------------

infoCmdSHelp :: String
infoCmdSHelp = "info [ARG..] : display summary of all bibliographies "
               ++ "and the current context"

infoCmdLHelp :: String
infoCmdLHelp = unlines hs
    where hs = [ infoCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Leave the current context unchanged."
               , "  2. Display any arguments supplied. You can use this to more"
               , "     easily track the context as the script runs."
               , "  4. Display what is currently in context."
               , "  5. List the working, import and export bibliographies with"
               , "     summary information.\n"
               , "See also <view>."
               ]

infoCmd :: T.CommandMonad T.Context
infoCmd xs rs = get >>= put . (addToLog . summarize xs rs <*> id) >> pure rs

-- view command -----------------------------------------------------

viewCmdSHelp :: String
viewCmdSHelp = "view [list] : view the details of all entries in the context"

viewCmdLHelp :: String
viewCmdLHelp = unlines hs
    where hs = [ viewCmdSHelp ++ "\n"
               , "This command has no other effect besides displaying the"
               , "entires in the context in a nicely formatted way. You can"
               , "also abbreviate the output by supplying the argument <list>."
               , "See also the <info> command."
               ]

viewCmd :: T.CommandMonad T.Context
viewCmd _ []          = get >>= put . addToLog msg >> pure []
    where msg = "\nNo entries to view.\n"
viewCmd ("list":_) rs = get >>= put . addToLog refList >> pure rs
    where refList = Tx.intercalate "\n" . map summarizeEntry $ rs
viewCmd _ rs          = get >>= put . addToLog refList >> pure rs
    where refList = Tx.intercalate "\n\n" . map viewRef $ rs

-- =============================================================== --
-- Context constructors

-- doi command ------------------------------------------------------

doiCmdSHelp :: String
doiCmdSHelp = "doi  [DOI..] : download an entry using the doi of its"
              ++ " publication"

doiCmdLHelp :: String
doiCmdLHelp = unlines hs
    where sp = map ( \ x -> replicate 9 ' ' <> fst x ) supported
          hs = [ doiCmdSHelp ++ "\n"
               , "This command populates the context with entries downloaded"
               , "using the digital-object-identifiers of the corresponding"
               , "publications. It has the following effects:\n"
               , "  1. Update working bibliography with the current context."
               , "  2. Download BibTeX entries from each doi provided, e.g.,"
               , "         doi 10.1016/bs.mie.2017.07.022"
               , "  3. Populate the context with the downloaded entries.\n"
               , "Non-ascii characters in the entry are replaced with '[?]'."
               , "If there is an error in downloading or parsing an entry"
               , "then a missing entry is added to the context in its place."
               ]

doiCmd :: T.CommandMonad T.Context
doiCmd xs rs = do
    updateIn rs
    lift . mapM getDoi $ xs

-- find command -----------------------------------------------------

findCmdSHelp :: String
findCmdSHelp = "find [EXP..] : find entries that match EXP and move "
               ++ "into the context"

findCmdLHelp :: String
findCmdLHelp = unlines hs
    where hs = [ findCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Update the working bibilography with the current context"
               , "     and clear the context."
               , "  2. Search the working bibliography for all enteries that"
               , "     contain matches to the input expressions."
               , "  3. Move the matching entries into the current context."
               , "  4. Delete the entries from the working bibliography making"
               , "     this more like <pull> rather than <get>. If nothing is"
               , "     done with the entries, then they are saved back"
               , "     unchanged. This allows you to remove entries from the"
               , "     working bibilography using a find/toss combination.\n"
               , "The expressions take the form of strings that are case-"
               , "sensitive and can use the following sub-subexpressions:\n"
               , "  .  : any non-newline character"
               , "  *  : zero or more of preceeding subexpression"
               , "  \\d : any digit"
               , "  \\D : any non-digit"
               , "  \\w : any alpha-numeric"
               , "  \\W : any non-alpha-numeric"
               , "  \\s : any space including newlines and tabs"
               , "  \\S : any non-space, non-tab or non-newline\n"
               , "Matching with '*' is greedy. Note that when entering scripts"
               , "via the command line, you may need to escape characters that"
               , "would otherwise be expanded by the shell. This can be done"
               , "using a backslash or single quotes, but keep in mind that"
               , "the single quotes may also need to be escaped.\n"
               , "For example:\n"
               , "  Find all entries with the string 'cats are great':"
               , "    find cats\\\\sare\\\\sgreat  (script at command line)"
               , "    find cats\\sare\\sgreat    (script run from a file)"
               , "    find \\'cats are great\\'  (script at command line)"
               , "    find 'cats are great'    (script run from a file)\n"
               , "  Find entries with strings like 'AB', 'A12B' or 'A1234B':"
               , "    find A\\\\d\\*B   (script at command line)"
               , "    find 'A\\d*B'   (script at command line)"
               , "    find A\\d*B     (script run from a file)\n"
               , "  Find all entries containing the string 'cats' or 'dogs':"
               , "    find cats dogs"
               ]

findCmd :: T.CommandMonad T.Context
findCmd xs rs = do
    updateIn rs
    btxState <- get
    let bib = T.inBib btxState
        rs' = searchRefs bib xs
    put btxState { T.inBib = bib { T.refs = deleteRefs ( T.refs bib ) rs' } }
    pure rs'

-- get command ------------------------------------------------------

getCmdSHelp :: String
getCmdSHelp = "get  [KEY..] : copy entries from "
              ++ "working bibliography to context"

getCmdLHelp :: String
getCmdLHelp = unlines hs
    where hs = [ getCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Update the working bibliography with the current context"
               , "     and then clear the context."
               , "  2. Repopulate the context with the entries in the working"
               , "     bibliography having the specified KEYs without further"
               , "     changing the working bibliography."
               , "  3. If a KEY is not found in the working bibliography, then"
               , "     no corresponding reference is copied to the context.\n"
               , "You can use the argument <all> (i.e., get all) to populate"
               , "the the context with all the entries in the working"
               , "bibliography. See also <pull>, which deletes the entries from"
               , "the working bibliography after moving them to the context,"
               , "and <take>, which applies to the import bibliography."
               ]

getCmd :: T.CommandMonad T.Context
getCmd ("all":_) rs = allKeysToArgs . T.inBib <$> get >>= flip getCmd rs
getCmd xs        rs = do updateIn rs
                         bib <- T.inBib <$> get
                         pure . map (getRef bib) $ xs

-- new command ------------------------------------------------------

newCmdSHelp :: String
newCmdSHelp = "new  [TYPE..] : populate context with template entries "
              ++ "of specified TYPEs"

newCmdLHelp :: String
newCmdLHelp = unlines hs
    where sp = unlines . map ( \ x -> replicate 9 ' ' <> fst x ) $ supported
          hs = [ newCmdSHelp ++ "\n"
               , "The command has the following effects:\n"
               , "  1. Update the working bibliography with the current context"
               , "     and then clear the context."
               , "  2. Add new entry templates to the context with the"
               , "     specified types."
               , "  3. Each entry will be given the key '"
                 ++ Tx.unpack genericKey ++ "n', where n is"
               , "     a number, so as to not conflict with any other key in"
               , "     the working bibliography."
               , "  4. The currently supported entry template types are:\n"
               ,       sp
               , "  5. If an entry type is not supported, then a 'misc' type"
               , "     entry is generated instead."
               ]

newCmd :: T.CommandMonad T.Context
newCmd xs rs = do
    updateIn rs
    bib <- T.inBib <$> get
    let n = genKeyNumber bib
    pure . templates n $ xs

-- pull command -----------------------------------------------------

pullCmdSHelp :: String
pullCmdSHelp = "pull [KEY..] : move entries from "
              ++ "working bibliography to context"

pullCmdLHelp :: String
pullCmdLHelp = unlines hs
    where hs = [ pullCmdSHelp ++ "\n"
               , "This command is the same as <get>; however the entries are"
               , "moved from the working bibliography to the context and thus"
               , "deleted from the working bibliography. Therefore, this"
               , "command can be used to remove entries from the working"
               , "bibliography using the script:\n"
               , "    pull [KEY..] and toss\n"
               , "You can move all entries from the working bibilography to the"
               , "context using the <all> keyword (i.e., pull all). See also"
               , "<get> and <toss>."
               ]

pullCmd :: T.CommandMonad T.Context
pullCmd xs rs = do
    rs' <- getCmd xs rs
    btxState <- get
    let bib = T.inBib btxState
    put btxState { T.inBib = bib { T.refs = deleteRefs ( T.refs bib ) rs' } }
    pure rs'

-- take command -----------------------------------------------------

takeCmdSHelp :: String
takeCmdSHelp = "take [KEY..] : copy entries from "
               ++ "import bibliography to context"

takeCmdLHelp :: String
takeCmdLHelp = unlines hs
    where hs = [ takeCmdSHelp ++ "\n"
               , "This command is the same as <get>; however the entries are"
               , "copied to the context from the import bibliography rather"
               , "than the working bibliography. If the import bibliography is"
               , "unset, then the context is just cleared after updating the"
               , "working bibliography first. You can also copy all the entries"
               , "in the import bibliography to the context using the <all>"
               , "keyword. For example, the script\n"
               , "    btx in Working.bib, from Import.bib, take all\n"
               , "will add all the entries in Import.bib to Working.bib. If"
               , "Working.bib did not previously exist, then this is the same"
               , "as copying Import.bib to Working.bib."
               ]

takeCmd :: T.CommandMonad T.Context
takeCmd ("all":_) rs = do xs <- maybe [] allKeysToArgs . T.fromBib <$> get
                          takeCmd xs rs
takeCmd xs        rs = do updateIn rs
                          btxState <- get
                          case T.fromBib btxState of
                               Nothing  -> pure []
                               Just bib -> pure . map ( getRef bib ) $ xs

-- =============================================================== --
-- Context operators

-- edit command -----------------------------------------------------

editCmdSHelp :: String
editCmdSHelp = "edit  EDITOR : edit entries in the context "
               ++ "using the external EDITOR"

editCmdLHelp :: String
editCmdLHelp = unlines hs
    where hs = [ editCmdSHelp ++ "\n"
               , "This command allows you to run an editor program on all the"
               , "bibliography entries currently in the context. The edited"
               , "entries remain in the context. You can use this to edit"
               , "fields, delete fields, add fields, change the key (as an"
               , "alternative to <name>), etc. Some things to keep in mind:\n"
               , "  1. If you edit the key following a <get> command and save"
               , "     back to the working bibliography, then the entry will"
               , "     be saved alongside its unedited previous version. This"
               , "     can be avoided by using <pull> instead of <get>."
               , "  2. If you edit an entry obtained using <get>, but do not"
               , "     edit the key, saving back will overwrite the previous"
               , "     unedited version with the same key."
               ]

editCmd :: T.CommandMonad T.Context
editCmd []     _  = throwError "An editor program must be specified.\n"
editCmd _      [] = pure []
editCmd (x:[]) rs = lift ( mapM (runExternal x) rs ) >>= pure
editCmd (x:xs) _  = throwError "Only one editor may be specified with <edit>.\n"

-- name command -----------------------------------------------------

nameCmdSHelp :: String
nameCmdSHelp = "name [KEY..] : change key names for entries in the context"

nameCmdLHelp :: String
nameCmdLHelp = unlines hs
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
               , "There must be a one-to-one matching between the names and the"
               , "entries in the context, which can be missing. If there is not"
               , "a one-to-one matching, then the context remains unchanged.\n"
               , "NOTE: This command only applies in-context, so it should be"
               , "used with <pull>, <send> or <take> commands to change the"
               , "name of the entry in a bibliography. If you use it with"
               , "<get>, then you will end up having a duplicate entry in the"
               , "working bibliography with two different keys."
               ]

nameCmd :: T.CommandMonad T.Context
nameCmd ns rs
    | nn == nr  = pure . zipWith go ns $ rs
    | otherwise = get >>= put . addToLog cannotRenameMsg >> pure rs
    where nn                      = length ns
          nr                      = length rs
          go n (T.Ref fp k v)     = T.Ref (newFp fp k) (Tx.pack n) v
          go n (T.Missing fp k e) = T.Missing fp k (e ++ newName n)
          newFp fp k              = fp ++ " (originally " ++ Tx.unpack k ++ ")"
          newName n               = ", tried to rename " ++ n
          cannotRenameMsg         = Tx.pack $ H.renameErr nn nr

-- send command -----------------------------------------------------

sendCmdSHelp :: String
sendCmdSHelp = "send : update export bibliography with the current context"

sendCmdLHelp :: String
sendCmdLHelp = unlines hs
    where hs = [ sendCmdSHelp ++ "\n"
               , "This command has the following effects:\n"
               , "  1. Update the export bibliography with the current context"
               , "     overwritting any references that have the same keys."
               , "  2. Depopulate the context."
               , "  3. If no export bibliography is set, then nothing happens"
               , "     besides depopulation of the context."
               , "  4. Ignore any arguments.\n"
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
sendCmd _         rs = updateTo rs >> pure []

-- toss command -----------------------------------------------------

tossCmdSHelp :: String
tossCmdSHelp = "toss [KEY..] : remove some or all entries from the context"

tossCmdLHelp :: String
tossCmdLHelp = unlines hs
    where hs = [ tossCmdSHelp ++ "\n"
               , "This command removes those entries with the specified keys"
               , "from the current context. This can be used to selectively"
               , "mask the export of entries between bibliographies or to"
               , "simply delete an entry from the working bibliography."
               , "For example, to export all entries but Cats2016 from file"
               , "Working.bib to Export.bib, you could use\n"
               , "    btx in Working.bib, get all, toss Cats2016 and send to"
                      ++ " Export.bib \n"
               , "The <all> keyword can be used to clear all entries in the"
               , "context. Used with <pull>, this can be used to delete entries"
               , "from the working bibliography. For example,\n"
               , "    btx in Working.bib, pull Cats2016 Dogs1984 and toss all\n"
               , "deletes Cats2016 and Dogs1984 from the working bibliography."
               ]

tossCmd :: T.CommandMonad T.Context
tossCmd ("all":_) rs = pure []
tossCmd xs        rs = pure . foldl' dropRefByKey rs . map Tx.pack $ xs

-- =============================================================== --
-- Errors and help

-- err command ------------------------------------------------------
-- This is the command for non-commands. It just throws an exception
-- and provides help strings for invalid commands.

errCmdLHelp :: String -> String
-- ^Used as an error message for help regarding an invalid command.
errCmdLHelp c = "<" ++ c ++ ">" ++ " is not a valid btx scripting command.\n"

errCmd :: String -> T.Command T.Context
errCmd c = T.Command "err" (\ _ _ -> throwError . H.cmdInvalidErr $ c)
                     [] (errCmdLHelp c)
