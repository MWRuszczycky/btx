{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module View.Help
    ( -- Routing help requests
      getHelp
      -- Error messages
    , argInvalidErr
    , cmdInvalidErr
    , missingFromBibErr
    , missingToBibErr
    , missingScriptErr
    , renameErr
    , sameToFromBibs
    , uniqueBibErr
    ) where

-- =============================================================== --
-- Help, errors and other information messages                     --
-- =============================================================== --

import qualified Model.Core.Types as T
import qualified Data.Text        as Tx
import qualified Paths_btx        as Paths
import qualified Data.Version     as Ver
import           Data.Text                 ( Text               )
import           Data.List                 ( intercalate, find  )
import           Model.Macros              ( embedFile, gitHash )
import           View.View                 ( style, padRight    )

-- =============================================================== -- 
-- Routing help requests

getHelp :: T.StyleMap -> [T.Command T.Context] -> [String] -> Text
getHelp sm cmds []            = mainHelp sm (map T.cmdHelp cmds)
getHelp sm cmds ("btx":_)     = getHelp sm cmds []
getHelp _  _    ("copying":_) = copyingStr
getHelp _  _    ("version":_) = versionStr
getHelp sm cmds xs            = Tx.intercalate "\n" . map (showHelp sm hs) $ xs
    where hs = (map T.cmdHelp cmds) <> keywords <> directives

-- =============================================================== --
-- Main help messages

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

showHelp :: T.StyleMap -> [T.HelpInfo] -> String -> Text
showHelp sm hs x = let name = Tx.pack x
                   in  maybe ( "No help for <" <> name <> ">\n" )
                             ( detailedHelp sm                  )
                       . find ( elem name . T.names ) $ hs

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

keywords :: [T.HelpInfo]
keywords = [ andHelp
           , allHelp
           ]

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

directives :: [T.HelpInfo]
directives = [ helpHelp
             , runHelp
             , versionHelp
             ]

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
               , "If no FILE-PATH is supplied to <run>, then btx attempts to"
               , "read commands piped or redirected in from standard input."
               ]

-- =============================================================== --
-- Help strings

copyingStr :: Tx.Text
copyingStr = $(embedFile "res/help/copying.txt")

versionStr :: Tx.Text
versionStr = "btx-" <> v <> "-dev-" <> g
    where v = Tx.pack . Ver.showVersion $ Paths.version
          g = Tx.take 7 $(gitHash)

-- =============================================================== --
-- Error messages

argInvalidErr :: String -> String -> T.ErrString
argInvalidErr c a = "Invalid argument for " <> c <> ": " <> a <> ".\n"

cmdInvalidErr :: String -> T.ErrString
cmdInvalidErr c = "Invalid command: " <> "<" <> c <> ">.\n"

missingFromBibErr :: T.ErrString
missingFromBibErr = "Use of <take> without an import bibliography being set.\n"

missingToBibErr :: T.ErrString
missingToBibErr = "Use of <send> without an export bibliography being set.\n"

missingScriptErr :: T.ErrString
missingScriptErr = "Script file required (Try: btx help in).\n"

renameErr :: Int -> Int -> T.ErrString
renameErr n r = intercalate "\n" es
    where es = [ "The entries cannot be renamed, because the number of"
               , "entries currently in the context (" <> show r
                  <> ") does not match"
               , "the number of new names supplied (" <> show n <> ")."
               ]

sameToFromBibs :: T.ErrString
sameToFromBibs = "Import and export bibliographies cannot have the same path.\n"

uniqueBibErr :: FilePath -> T.ErrString
uniqueBibErr fp = unlines es
    where es = [ "Cannot find a unique default .bib file in the current"
                 <> " directory:"
               , fp
               , "(Try: btx help in)"
               ]
