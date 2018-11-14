{-# LANGUAGE OverloadedStrings #-}

module Help
    ( copyingHelpStr
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
    ) where

import Data.List ( intercalate )

-- =============================================================== --
-- General help strings

keywordHelpStr :: String
keywordHelpStr = intercalate "\n" hs
    where hs = [ "and  : separates btx commands"
               , "with : eliminate preceding <and> or its equivalent"
               , "all  : apply command to all entries in the context or "
                 ++ "bibliography"
               ]

directiveHelpStr :: String
directiveHelpStr = intercalate "\n" hs
    where hs = [ "run [FILE-PATH] : run btx script from a file"
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
    where hs = [ "run [FILE-PATH] : run btx script from a file\n"
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
helpStrHeader = "Welcome to btx! btx is a light-weight, declarative, command"
                 ++ " line interface for\nworking with BibTeX"
                 ++ " bibliography files."

helpStrFooter :: String
helpStrFooter = intercalate "\n" hs
    where hs = [ "btx is free, open-source software maintained with full"
                 ++ " documentation and\n  licensing information at:"
                 ++ " https://github.com/MWRuszczycky/btx\n"
               , "For binary copyright information, try: btx help copying"
               ]

versionStr :: String
versionStr = "btx version 0.1.0.0"

versionHelpStr :: String
versionHelpStr = "display version information"

-- =============================================================== --
-- License string

copyingHelpStr :: String
copyingHelpStr = intercalate "\n"
  [ "The btx binary contains code generated from the following sources. Each of"
  , "the licenses represented are reproduced in full below.\n"
  , "btx | BSD-3"
  , "    (c) Mark W. Ruszczycky 2018"
  , "    Author: Mark W. Ruszczycky"
  , "    https://github.com/MWRuszczycky/btx\n"
  , "GHC-8.4.3 | GHC-License"
  , "    (c) The University Court of the University of Glasgow 2004"
  , "    https://www.haskell.org/ghc\n"
  , "stack | BSD-3"
  , "    (c) Stack contributors 2015-2018"
  , "    https://docs.haskellstack.org/en/stable/README/"
  , "    https://github.com/commercialhaskell/stack\n"
  , "base-4.11.1.0 | GHC-License, SPJ-Haskell98 & MMTC-Haskell98"
  , "    GHC-License : (c) The University Court of the University of Glasgow 2004"
  , "    SPJ-Haskell98 : (c) Simon Peyton Jones 2002"
  , "    MMTC-Haskell98 : (c) Manuel M. T. Chakravarty 2002"
  , "    https://hackage.haskell.org/package/base\n"
  , "attoparsec-0.13.2.2 | BSD-3"
  , "    (c) Lennart Kolmodin"
  , "    Author: Bryan O'Sullivan"
  , "    Maintainers: Bryan O'Sullivan, Ben Gamari"
  , "    https://hackage.haskell.org/package/attoparsec\n"
  , "bytestring-0.10.8.2 | BSD-3"
  , "    (c) Don Stewart 2005-2009"
  , "    (c) Duncan Coutts 2006-2015"
  , "    (c) David Roundy 2003-2005"
  , "    (c) Simon Meier 2010-2011"
  , "    Authors: Don Stewart, Duncan Coutts"
  , "    Maintainer: Duncan Coutts"
  , "    https://hackage.haskell.org/package/bytestring\n"
  , "containers-0.5.11.0 | GHC-License"
  , "    (c) The University Court of the University of Glasgow 2004"
  , "    https://hackage.haskell.org/package/containers\n"
  , "directory-1.3.1.5 | GHC-License & SPJ-Haskell98"
  , "    GHC-License : (c) The University Court of the University of Glasgow 2004"
  , "    SPJ-Haskell98 : (c) Simon Peyton Jones 2002"
  , "    https://hackage.haskell.org/package/directory\n"
  , "microlens-0.4.9.1 | BSD-3"
  , "    (c) Edward Kmett 2013-2016"
  , "    (c) Artyom Kazak 2015-2016"
  , "    Authors: Edward Kmett, Artyom Kazak"
  , "    Maintainer: Monadfix"
  , "    https://hackage.haskell.org/package/microlens\n"
  , "mtl-2.2.2 | GHC-License/BSD-3-style"
  , "    GHC-License (c) The University Court of the University of Glasgow 2004"
  , "    Author: Andy Gill"
  , "    Maintainer: Edward Kmett"
  , "    https://hackage.haskell.org/package/mtl\n"
  , "process-1.6.3.0 | GHC-License & SPJ-Haskell98:"
  , "    GHC-License : (c) The University Court of the University of Glasgow 2004"
  , "    SPJ-Haskell98 : (c) Simon Peyton Jones 2002"
  , "    https://hackage.haskell.org/package/process\n"
  , "temporary-1.3 | BSD-3"
  , "    (c) Isaac Jones   2003-2006"
  , "    (c) Duncan Coutts 2005-2009"
  , "    (c) Maximilian Bolingbroke 2008"
  , "        ... and other contributors"
  , "    Maintainers: Mateusz Kowalczyk, Roman Cheplyaka"
  , "    https://hackage.haskell.org/package/temporary\n"
  , "text-1.2.3.0 | BSD-2"
  , "    (c) Tom Harper 2008-2009"
  , "    (c) Bryan O'Sullivan 2009-2011"
  , "    Author: Bryan O'Sullivan"
  , "    Maintainer: Bryan O'Sullivan"
  , "    https://hackage.haskell.org/package/text\n"
  , "wreq-0.5.2.1 | BSD-3"
  , "    (c) Bryan O'Sullivan 2014"
  , "    Author: Bryan O'Sullivan"
  , "    Maintainer: Bryan O'Sullivan"
  , "    https://hackage.haskell.org/package/wreq\n"
  , "\nDetails of each license type:\n"
  , replicate 80 '-'
  , ghcLicense
  , replicate 80 '-'
  , spj2002
  , replicate 80 '-'
  , mmtc2002
  , replicate 80 '-'
  , bsd2License
  , replicate 80 '-'
  , bsd3License
  ]

bsd2License :: String
bsd2License = unlines
  [ "BSD-2\n"
  , "Redistribution and use in source and binary forms, with or without"
  , "modification, are permitted provided that the following conditions"
  , "are met:\n"
  , "    1. Redistributions of source code must retain the above copyright\n"
  , "       notice, this list of conditions and the following disclaimer.\n"
  , "    2. Redistributions in binary form must reproduce the above"
  , "       copyright notice, this list of conditions and the following"
  , "       disclaimer in the documentation and/or other materials provided"
  , "       with the distribution.\n"
  , "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS"
  , "``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT"
  , "LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR"
  , "A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT"
  , "OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,"
  , "SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT"
  , "LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,"
  , "DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY"
  , "THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT"
  , "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE"
  , "OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
  ]

bsd3License :: String
bsd3License = unlines
  [ "BSD-3\n"
  , "Redistribution and use in source and binary forms, with or without"
  , "modification, are permitted provided that the following conditions"
  , "are met:\n"
  , "    1. Redistributions of source code must retain the above copyright"
  , "       notice, this list of conditions and the following disclaimer.\n"
  , "    2. Redistributions in binary form must reproduce the above copyright\n"
  , "       notice, this list of conditions and the following disclaimer in the"
  , "       documentation and/or other materials provided with the distribution.\n"
  , "    3. Neither the name of the author nor the names of his contributors"
  , "       may be used to endorse or promote products derived from this software"
  , "       without specific prior written permission.\n"
  , "THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS"
  , "OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED"
  , "WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE"
  , "DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR"
  , "ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL"
  , "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS"
  , "OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)"
  , "HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,"
  , "STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN"
  , "ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE"
  , "POSSIBILITY OF SUCH DAMAGE."
  ]

ghcLicense :: String
ghcLicense = unlines
  [ "The Glasgow Haskell Compiler License\n"
  , "Copyright 2004, The University Court of the University of Glasgow."
  , "All rights reserved.\n"
  , "Redistribution and use in source and binary forms, with or without"
  , "modification, are permitted provided that the following conditions are met:\n"
  , "    - Redistributions of source code must retain the above copyright notice,"
  , "    this list of conditions and the following disclaimer.\n"
  , "    - Redistributions in binary form must reproduce the above copyright notice,"
  , "    this list of conditions and the following disclaimer in the documentation"
  , "    and/or other materials provided with the distribution.\n"
  , "    - Neither name of the University nor the names of its contributors may be"
  , "    used to endorse or promote products derived from this software without"
  , "    specific prior written permission.\n"
  , "THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF"
  , "GLASGOW AND THE CONTRIBUTORS 'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES,"
  , "INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND"
  , "FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE"
  , "UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE"
  , "FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL"
  , "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR"
  , "SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER"
  , "CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT"
  , "LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY"
  , "OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH"
  , "DAMAGE."
  ]

spj2002 :: String
spj2002 = unlines
  [ "SPJ-Haskell98\n"
  , "Code derived from the document 'Report on the Programming Language"
  , "Haskell 98', is distributed under the following license:\n"
  , "  Copyright (c) 2002 Simon Peyton Jones\n"
  , "  The authors intend this Report to belong to the entire Haskell"
  , "  community, and so we grant permission to copy and distribute it for"
  , "  any purpose, provided that it is reproduced in its entirety,"
  , "  including this Notice.  Modified versions of this Report may also be"
  , "  copied and distributed for any purpose, provided that the modified"
  , "  version is clearly presented as such, and that it does not claim to"
  , "  be a definition of the Haskell 98 Language."
  ]

mmtc2002 :: String
mmtc2002 = unlines
  [ "MMTC-Haskell98\n"
  , "Code derived from the document 'The Haskell 98 Foreign Function"
  , "Interface, An Addendum to the Haskell 98 Report' is distributed under"
  , "the following license:\n"
  , "  Copyright (c) 2002 Manuel M. T. Chakravarty\n"
  , "  The authors intend this Report to belong to the entire Haskell"
  , "  community, and so we grant permission to copy and distribute it for"
  , "  any purpose, provided that it is reproduced in its entirety,"
  , "  including this Notice.  Modified versions of this Report may also be"
  , "  copied and distributed for any purpose, provided that the modified"
  , "  version is clearly presented as such, and that it does not claim to"
  , "  be a definition of the Haskell 98 Foreign Function Interface."
  ]
