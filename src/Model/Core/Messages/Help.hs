{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Messages.Help
    ( -- General help messages
      displayHelp
    -- Keyword help strings
    , keywordHelp
    -- Directive help strings
    , directiveHelp
    -- Error messages
    , argInvalidErr
    , cmdInvalidErr
    , invalidUsageErr
    , missingScriptErr
    , noCommandsErr
    , renameErr
    , unableToParseErr
    , uniqueBibErr
    -- Help formatting
    , shortCmdHelpStr
    , longCmdHelpStr
    ) where

-- =============================================================== --
-- Help, errors and other information messages
-- =============================================================== --

import qualified Model.Core.Types      as T
import Data.Text                             ( pack, unpack        )
import Data.List                             ( intercalate, find   )
import Model.Core.Formatting                 ( style               )

-- =============================================================== --
-- Local helper types

data DirectiveHelp = DirectiveHelp {
      dirName  :: String
    , dirArgs  :: String
    , dirShort :: String
    , dirLong  :: String
    }

data KeywordHelp = KeywordHelp {
      kwName  :: String
    , kwShort :: String
    , kwLong  :: String
    }

-- =============================================================== --
-- General help messages

displayHelp :: T.StyleMap -> [T.Command T.Context] -> String
displayHelp sm xs = unlines hs
    where hs = [ helpStrHeader sm
               , section sm "usage"
               , "btx DIRECTIVE | SCRIPT"
               , section sm "directives"              , directiveSummaries sm
               , section sm "script keyword summaries", keywordSummaries sm
               , section sm "script command summaries", summarizeCommands sm xs
               , section sm "copying"                 , helpStrFooter
               ]

helpStrHeader :: T.StyleMap -> String
helpStrHeader sm = unpack (style sm "emph" "Welcome to btx!")
                   ++ " btx is a light-weight, command-line interface"
                   ++ " for working with\nBibTeX bibliography files."

helpStrFooter :: String
helpStrFooter = intercalate "\n" hs
    where hs = [ "btx is free, open-source software maintained with full"
                 ++ " documentation and\n  licensing information at:"
                 ++ " https://github.com/MWRuszczycky/btx\n"
               , "For binary copyright information, try: btx help copying"
               ]

section :: T.StyleMap -> String -> String
section sm name =
     let n  = length name
         ds = replicate (80 - n - 5) '-'
         nm = pack name
     in  "\n-- " ++ unpack (style sm "header" nm) ++ " " ++ ds

summarizeCommands :: T.StyleMap -> [T.Command T.Context] -> String
summarizeCommands sm xs =
    let nLen = maximum . map ( length . T.cmdName ) $ xs
        aLen = maximum . map ( length . T.cmdArgs ) $ xs
    in  intercalate "\n" . map (shortCmdHelpStr sm nLen aLen) $ xs

directiveSummaries :: T.StyleMap -> String
directiveSummaries sm =
    let nLen = maximum . map ( length . dirName ) $ directives
        aLen = maximum . map ( length . dirArgs ) $ directives
    in  intercalate "\n" . map (shortDirHelpStr sm nLen aLen) $ directives

keywordSummaries :: T.StyleMap -> String
keywordSummaries sm =
    let nLen = maximum . map ( length . kwName ) $ keywords
    in  intercalate "\n" . map ( shortKwHelpStr sm nLen ) $ keywords

-- =============================================================== --
-- Keyword help strings

---------------------------------------------------------------------
-- Formatting

keywordHelp :: T.StyleMap -> String -> String
keywordHelp sm x = maybe ("No such keyword " ++ x) (longKwHelpStr sm)
                   . find ( (== x) . kwName )
                   $ keywords

shortKwHelpStr :: T.StyleMap -> Int -> KeywordHelp -> String
shortKwHelpStr sm padName (KeywordHelp name helpStr _) =
    shortHelpStr sm (padName, name) (0, "") helpStr

longKwHelpStr :: T.StyleMap -> KeywordHelp -> String
longKwHelpStr sm kw
    | null $ kwLong kw = hdr ++ "\n"
    | otherwise        = hdr ++ bdy
    where hdr = shortKwHelpStr sm 0 kw
          bdy = "\n\n" ++ kwLong kw

---------------------------------------------------------------------
-- Help strings

keywords :: [KeywordHelp]
keywords = [ andHelp
           , withHelp
           , allHelp
           ]

andHelp :: KeywordHelp
andHelp = KeywordHelp n s (unlines l)
    where n = "and"
          s = "separates btx commands (same as ',' and '\\n')"
          l = [ "The <and> keyword is used to separate btx scripting"
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

withHelp :: KeywordHelp
withHelp = KeywordHelp n s (unlines l)
    where n = "with"
          s = "eliminate preceding <and> or its equivalent (same as '+')"
          l = [ "The <with> keyword is used to eliminate an <and> or its"
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

allHelp :: KeywordHelp
allHelp = KeywordHelp n s (unlines l)
    where n = "all"
          s =  "apply command to all entries in the context or bibliography"
          l = [ "The <all> keyword can be supplied to the commands <get>,"
               , "<pull> and <take> so that they apply to all entries in a"
               , "given bibliography. For example,\n"
               , "  get all\n"
               , "will populate the context with all entries in the working"
               , "bibliography. See help for these individual commands for"
               , "more details."
               ]

-- =============================================================== --
-- Directive help strings

---------------------------------------------------------------------
-- Formatting

directiveHelp :: T.StyleMap -> String -> String
directiveHelp sm x = maybe ("No such directive " ++ x) (longDirHelpStr sm)
                     . find ( (== x) . dirName )
                     $ directives

shortDirHelpStr :: T.StyleMap -> Int -> Int -> DirectiveHelp -> String
shortDirHelpStr sm padName padArgs (DirectiveHelp name args helpStr _) =
    shortHelpStr sm (padName, name) (padArgs, args) helpStr

longDirHelpStr :: T.StyleMap -> DirectiveHelp -> String
longDirHelpStr sm d
    | null $ dirLong d = hdr ++ "\n"
    | otherwise        = hdr ++ bdy
    where hdr = shortDirHelpStr sm 0 0 d
          bdy = "\n\n" ++ dirLong d

---------------------------------------------------------------------
-- Help strings

directives :: [DirectiveHelp]
directives = [ helpHelp
             , runHelp
             , versionHelp
             ]

helpHelp :: DirectiveHelp
helpHelp = DirectiveHelp n a s ""
    where n = "help"
          a = "ARGUMENT"
          s = "show this help screen or more help for ARGUMENT"

versionHelp :: DirectiveHelp
versionHelp = DirectiveHelp n "" s ""
    where n = "version"
          s = "display version information"

runHelp :: DirectiveHelp
runHelp = DirectiveHelp n a s (unlines l)
    where n = "run"
          a = "FILE-PATH"
          s = "run btx script from a file"
          l = [ "Rather than run a script entered at the command line, you"
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

unableToParseErr :: T.ErrString
unableToParseErr  = "Unable to parse input. (Try: btx help)\n"

uniqueBibErr :: FilePath -> T.ErrString
uniqueBibErr fp = unlines es
    where es = [ "Cannot find a unique default .bib file in the current"
                 ++ " directory:"
               , fp
               , "(Try: btx help in)"
               ]

-- =============================================================== --
-- General help formatting

padRightStr :: Int -> String -> String
padRightStr n x = x ++ replicate (n - length x) ' '

shortHelpStr :: T.StyleMap -> (Int,String) -> (Int,String) -> String -> String
shortHelpStr sm (padName, name) (padArgs, args) helpStr =
    let nm = style sm "emph"    . pack . padRightStr padName $ name
        hs = style sm "command" . pack $ helpStr
        as = padRightStr padArgs args
    in  unpack nm ++ " " ++ as ++ " : " ++ unpack hs

shortCmdHelpStr :: T.StyleMap -> Int -> Int -> T.Command T.Context -> String
shortCmdHelpStr sm namePad argPad (T.Command name _ args helpStr _) =
    shortHelpStr sm (namePad, name) (argPad, args) helpStr

longCmdHelpStr :: T.StyleMap -> T.Command T.Context -> String
longCmdHelpStr sm c = shortCmdHelpStr sm 0 0 c ++ "\n\n" ++ T.cmdLHelp c
