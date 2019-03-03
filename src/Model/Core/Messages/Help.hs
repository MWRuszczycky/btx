module Model.Core.Messages.Help
    ( -- General help messages
      displayHelp
    , directiveHelpStr
    , helpStrHeader
    , helpStrFooter
    , keywordHelpStr
    , andHelpStr
    , withHelpStr
    , allHelpStr
    , runHelpStr
    , versionHelpStr
    , versionStr
    -- Error messages
    , argInvalidErr
    , cmdInvalidErr
    , invalidUsageErr
    , missingScriptErr
    , noCommandsErr
    , renameErr
    , unableToParseErr
    , uniqueBibErr
    ) where

-- =============================================================== --
-- Help, errors and other information messages
-- =============================================================== --

import qualified Model.Core.Types as T
import Data.List                        ( intercalate
                                        , sort        )
import Data.Version                     ( showVersion )
import Paths_btx                        ( version     )

-- =============================================================== --
-- General help messages

displayHelp :: [T.Command T.Context] -> String
displayHelp xs = unlines hs
    where ds = replicate 53 '-'
          hs = [ helpStrHeader
               , "\n-- usage -----------------" ++ ds
               , "btx [run FILE-PATH | help [COMMAND] | version] [SCRIPT]"
               , "\n-- btx directives --------" ++ ds, directiveHelpStr
               , "\n-- btx keyword summaries -" ++ ds, keywordHelpStr
               , "\n-- btx command summaries -" ++ ds, summarizeCommands xs
               , "\n-- copying ---------------" ++ ds, helpStrFooter
               ]

summarizeCommands :: [T.Command T.Context] -> String
summarizeCommands xs = intercalate "\n" . map go $ xs'
    where xs'      = map ( break (== ':') ) . sort . map T.cmdSHelp $ xs
          n        = maximum . map ( length . fst ) $ xs'
          go (c,s) = padRightStr n c ++ s

keywordHelpStr :: String
keywordHelpStr = intercalate "\n" hs
    where hs = [ "and  : separates btx commands"
               , "with : eliminate preceding <and> or its equivalent"
               , "all  : apply command to all entries in the context or "
                 ++ "bibliography"
               ]

directiveHelpStr :: String
directiveHelpStr = intercalate "\n" hs
    where hs = [ "run  FILE-PATH  : run btx script from a file"
               , "help [ARGUMENT] : show this help screen or more help for"
                 ++ " ARGUMENT"
               , "version         : display version information"
               ]

andHelpStr :: String
andHelpStr = unlines hs
    where hs = [ "and  : separates btx commands (same as ',' and '\\n')\n"
               , "The <and> keyword is used to separate btx scripting"
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

withHelpStr :: String
withHelpStr = unlines hs
    where hs = [ "with : eliminate preceding <and> or its equivalent (same as"
                 ++ " '+')\n"
               , "The <with> keyword is used to eliminate an <and> or its"
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

allHelpStr :: String
allHelpStr = unlines hs
    where hs = [ "all  : apply command to all entries in the context or "
                 ++ "bibliography\n"
               , "The <all> keyword can be supplied to the commands <get>,"
               , "<pull>, <list>, and <take> so that they apply to all entries"
               , "is a given bibliography. It can be supplied to <toss> so"
               , "that <toss> applies to all entries in the current context."
               , "See help for the individual command for more details."
               ]

runHelpStr :: String
runHelpStr = unlines hs
    where hs = [ "run FILE-PATH : run btx script from a file\n"
               , "Rather than run a script entered at the command line, you"
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

helpStrHeader :: String
helpStrHeader = "Welcome to btx! btx is a light-weight, command-line interface"
                 ++ " for working with\nBibTeX bibliography files."

helpStrFooter :: String
helpStrFooter = intercalate "\n" hs
    where hs = [ "btx is free, open-source software maintained with full"
                 ++ " documentation and\n  licensing information at:"
                 ++ " https://github.com/MWRuszczycky/btx\n"
               , "For binary copyright information, try: btx help copying"
               ]

versionStr :: String
versionStr = "btx version " ++ showVersion version ++ "\n"

versionHelpStr :: String
versionHelpStr = "display version information\n"

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
