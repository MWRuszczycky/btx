{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Messages.Help
    ( -- Main help messages
      mainHelp
      -- Help Formatting
    , showHelp
      -- Help for keywords
    , keywords
      -- Help for directives
    , directives
      -- Error messages
    , argInvalidErr
    , cmdInvalidErr
    , invalidUsageErr
    , missingFromBibErr
    , missingToBibErr
    , missingScriptErr
    , noCommandsErr
    , renameErr
    , sameToFromBibs
    , unableToParseErr
    , uniqueBibErr
    ) where

-- =============================================================== --
-- Help, errors and other information messages                     --
-- =============================================================== --

import qualified Model.Core.Types      as T
import qualified Data.Text             as Tx
import Data.Text                             ( Text                )
import Data.List                             ( intercalate, find   )
import Model.Core.Formatting                 ( style, padRight     )

-- =============================================================== --
-- Main help messages

-- Exported

mainHelp :: T.StyleMap -> [T.HelpInfo] -> Text
mainHelp sm commands = Tx.unlines
    [ mainHelpHeader sm
    , section sm "usage"
    , "btx DIRECTIVE | SCRIPT"
    , section sm "directives"              , summaryList sm directives
    , section sm "script keyword summaries", summaryList sm keywords
    , section sm "script command summaries", summaryList sm commands
    , section sm "copying"                 , mainHelpFooter
    ]

-- Unexported

mainHelpHeader :: T.StyleMap -> Text
mainHelpHeader sm = Tx.unwords
    [ style sm "emph" "Welcome to btx!"
    , "btx is a light-weight, command-line interface"
    , "for working with\nBibTeX bibliography files."
    ]

mainHelpFooter :: Text
mainHelpFooter = Tx.unwords
    [ "btx is free, open-source software maintained with full"
    , "documentation and\n  licensing information at:"
    , "https://github.com/MWRuszczycky/btx\n\nFor binary copyright information,"
    , "try: btx help copying"
    ]

section :: T.StyleMap -> Text -> Text
section sm name = Tx.unwords
    [ "\n--"
    , style sm "header" name
    , Tx.replicate (80 - Tx.length name - 5) "-"
    ]

-- =============================================================== --
-- Help Formatting

-- Exported

showHelp :: T.StyleMap -> [T.HelpInfo] -> String -> Text
showHelp sm hs x = let name = Tx.pack x
                   in  maybe ( "No help for <" <> name <> ">\n" )
                             ( detailedHelp sm                  )
                       . find ( elem name . T.names ) $ hs

-- Unexported

formatAllNames :: T.StyleMap -> Int -> T.HelpInfo -> Text
formatAllNames sm width h = padRight width ns
    where ns = Tx.intercalate " | " . map (style sm "emph") . T.names $ h

formatMainName :: T.StyleMap -> Int -> T.HelpInfo -> Text
formatMainName sm width h = case T.names h of
                                 []    -> padRight width Tx.empty
                                 (n:_) -> style sm "emph" . padRight width $ n

detailedHelp :: T.StyleMap -> T.HelpInfo -> Text
detailedHelp sm h
    | Tx.null $ T.longHelp h = hdr <> "\n"
    | otherwise              = hdr <> bdy
    where bdy = "\n\n" <> T.longHelp h
          hdr = Tx.unwords [ formatAllNames sm 0 h
                           , T.usage h
                           , ":"
                           , style sm "short" . T.shortHelp $ h
                           ]

summaryHelp :: T.StyleMap -> Int -> Int -> T.HelpInfo -> Text
summaryHelp sm nw uw h = Tx.unwords [ formatMainName sm nw h
                                    , padRight uw . T.usage $ h
                                    , ":"
                                    , style sm "short" . T.shortHelp $ h
                                    ]

summaryList :: T.StyleMap -> [T.HelpInfo] -> Text
summaryList sm hs =
    let go (T.HelpInfo []    _ _ _ ) = 0
        go (T.HelpInfo (n:_) _ _ _ ) = Tx.length n
        nLen = maximum . map go $ hs
        uLen = maximum . map ( Tx.length . T.usage ) $ hs
    in  Tx.intercalate "\n" . map ( summaryHelp sm nLen uLen ) $ hs

-- =============================================================== --
-- Help for keywords

-- Exported

keywords :: [T.HelpInfo]
keywords = [ andHelp
           , withHelp
           , allHelp
           ]

-- Unexported

andHelp :: T.HelpInfo
andHelp = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "and", "\\n", "," ]
          us = Tx.empty
          sh = "Separates btx commands"
          lh = [ "The <and> keyword is used to separate btx scripting"
               , "commands in a btx script. For example,\n"
               , "    btx in animals.bib and get Cats2016 and view\n"
               , "The <and> keyword can be abbreviated in two ways. The first"
               , "is a comma, so that the above is the same as\n"
               , "    btx in animals.bib, get Cats2016, view\n"
               , "The second is a line break, which is useful for writing"
               , "scripts as separate files. Continuing with the example:\n"
               , "    in animals.bib"
               , "        get Cats2016"
               , "        view"
               ]

withHelp :: T.HelpInfo
withHelp = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "with", "+" ]
          us = Tx.empty
          sh = "Eliminate preceding <and> or its equivalent"
          lh = [ "The <with> keyword is used to eliminate an <and> or its"
               , "equivalent that immediately precedes it. This can be used to"
               , "more easily pass a large number of arguments to a single btx"
               , "scripting command. For example, if we have a file containing"
               , "the script:\n"
               , "    in animals.bib, get Cats Dogs Chipmunks, view\n"
               , "then we can separate the Cats, Dogs and Chipmunks arguments"
               , "onto separate lines using <with>:\n"
               , "    in animals.bib"
               , "        get Cats"
               , "           with Dogs"
               , "           with Chipmunks"
               , "        view\n"
               , "since the new lines are parsed as <and> keywords. The <with>"
               , "keyword can also be abbreviated with a '+'. Thus the above"
               , "script is also equivalent to:\n"
               , "    in animals.bib"
               , "        get Cats"
               , "           + Dogs"
               , "           + Chipmunks"
               , "        view\n"
               , "If we did not use <with>, then Dogs and Chipmunks would have"
               , "been interpreted as scripting commands."
               ]

allHelp :: T.HelpInfo
allHelp = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = ["all"]
          us = Tx.empty
          sh =  "Apply command to all entries in the context or bibliography"
          lh = [ "The <all> keyword can be supplied to the commands <get>,"
               , "<pull> and <take> so that they apply to all entries in a"
               , "given bibliography. For example,\n"
               , "  get all\n"
               , "will populate the context with all entries in the working"
               , "bibliography. See help for these individual commands for"
               , "more details."
               ]

-- =============================================================== --
-- Help for directives

-- Exported

directives :: [T.HelpInfo]
directives = [ helpHelp
             , runHelp
             , versionHelp
             ]

-- Unexported

helpHelp :: T.HelpInfo
helpHelp = T.HelpInfo ns us sh Tx.empty
    where ns = [ "help" ]
          us = "[ARGUMENT]"
          sh = "Show main help screen or more help for ARGUMENT"

versionHelp :: T.HelpInfo
versionHelp = T.HelpInfo ns Tx.empty sh Tx.empty
    where ns = [ "version" ]
          sh = "Display version information"

runHelp :: T.HelpInfo
runHelp = T.HelpInfo ns us sh (Tx.unlines lh)
    where ns = [ "run" ]
          us = "FILE-PATH"
          sh = "Run btx script from a file"
          lh = [ "Rather than run a script entered at the command line, you"
               , "can use the <run> directive to run a script from a text file."
               , "In this case you can take advantage of line breaks and the"
               , "<with> command to better lay out the script. Line breaks are"
               , "directly interpreted as <and> keywords (see help and). White-"
               , "space is otherwise ignored. If no FILE-PATH is supplied to"
               , "<run>, then btx attempts to read commands from standard input"
               , "producing a REPL-like interpreter. To exit this interactive"
               , "editing of bibliographies, use <ctrl-c> after saving your"
               , "work with the <save> command (see also help for the <and>,"
               , "<save> and <with> keywords and commands)."
               ]

-- =============================================================== --
-- Error messages

argInvalidErr :: String -> String -> T.ErrString
argInvalidErr c a = "Invalid argument for " ++ c ++ ": " ++ a ++ ".\n"

cmdInvalidErr :: String -> T.ErrString
cmdInvalidErr c = "Invalid command: " ++ "<" ++ c ++ ">.\n"

invalidUsageErr :: String -> T.ErrString
invalidUsageErr c = unwords [ "Invalid usage for command <" ++ c ++ ">."
                            , "(Try: btx help " ++ c ++ ")\n"
                            ]

missingFromBibErr :: T.ErrString
missingFromBibErr = "Use of <take> without an import bibliography being set.\n"

missingToBibErr :: T.ErrString
missingToBibErr = "Use of <send> without an export bibliography being set.\n"

missingScriptErr :: T.ErrString
missingScriptErr = "Script file required (Try: btx help in).\n"

noCommandsErr :: T.ErrString
noCommandsErr   = unwords [ "This won't do anything --"
                          , "a script or command needs to be provided."
                          , "(Try: btx help)\n"
                          ]

renameErr :: Int -> Int -> T.ErrString
renameErr n r = intercalate "\n" es
    where es = [ "The entries cannot be renamed, because the number of"
               , "entries currently in the context (" ++ show r
                  ++ ") does not match"
               , "the number of new names supplied (" ++ show n ++ ")."
               ]

sameToFromBibs :: T.ErrString
sameToFromBibs = "Import and export bibliographies cannot have the same path.\n"

unableToParseErr :: T.ErrString
unableToParseErr  = "Unable to parse input. (Try: btx help)\n"

uniqueBibErr :: FilePath -> T.ErrString
uniqueBibErr fp = unlines es
    where es = [ "Cannot find a unique default .bib file in the current"
                 ++ " directory:"
               , fp
               , "(Try: btx help in)"
               ]
