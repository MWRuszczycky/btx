module Help
    ( copyingHelpStr
    , helpStr
    , andHelpStr
    , withHelpStr
    , allHelpStr
    , runHelpStr
    ) where

-- =============================================================== --
-- General help strings

andHelpStr :: String
andHelpStr = unlines hs
    where hs = [ "and/,/\\n  : separates btx commands\n"
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
    where hs = [ "with/+ : eliminate preceding <and> or its equivalent\n"
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
runHelpStr = "help for <run>\n"

helpStr :: String
helpStr = "General help string.\n"


-- =============================================================== --
-- License string
-- to-do

copyingHelpStr :: String
copyingHelpStr = "License information will go here."
