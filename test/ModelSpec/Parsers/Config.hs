{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the 'parsing' test-suite for testing the                --
-- Model.Parsers.Scripts module
-- =============================================================== --

import qualified Model.Core.Types      as T
import qualified Data.Text             as Tx
import           Model.Parsers.Config        ( parseScript   )
import           Data.Text                   ( Text          )
import           Data.Either                 ( isLeft        )
import           Test.Hspec                  ( Spec (..)
                                             , describe
                                             , hspec
                                             , it
                                             , shouldBe
                                             , shouldSatisfy )

main :: IO ()
main = hspec $ do
    describe "The btx script-parser" $ do
        it "handles simple scripts" $ do
            parseScript script101 `shouldBe` result101
            parseScript script102 `shouldBe` result102
            parseScript script103 `shouldBe` result103
            parseScript script104 `shouldBe` result104
        it "handles variations \\n and '+' correctly" $ do
            parseScript script201 `shouldBe` result201
            parseScript script202 `shouldBe` result202
            parseScript script203 `shouldBe` result203
            parseScript script204 `shouldBe` result204
            parseScript script205 `shouldBe` result205
            parseScript script206 `shouldBe` result206
        it "handles quoted strings correctly" $ do
            parseScript script301 `shouldBe` result301
            parseScript script302 `shouldBe` result302
            parseScript script303 `shouldBe` result303
            parseScript script304 `shouldBe` result304
            parseScript script305 `shouldBe` result305
            parseScript script306 `shouldBe` result306
            parseScript script307 `shouldBe` result307
            parseScript script308 `shouldBe` result308
        it "handles malformed scripts" $ do
            parseScript script401 `shouldSatisfy` isLeft
            parseScript script402 `shouldSatisfy` isLeft
            parseScript script403 `shouldSatisfy` isLeft
            parseScript script404 `shouldSatisfy` isLeft

-- =============================================================== --
-- Test scripts

---------------------------------------------------------------------
-- Simple scripts

script101 = "in animals.bib, pull Cats2016, name Felines2018 and view"
result101 = Right ( Just "animals.bib"
                  , [ ( "pull", ["Cats2016"]    )
                    , ( "name", ["Felines2018"] )
                    , ( "view", []              )
                    , ( "save", []              )
                    ] )

script102 = "in animals.bib, pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result102 = Right ( Just "animals.bib"
                  , [ ( "pull", ["Cats2016", "Dogs1977"]    )
                    , ( "name", ["Felines2018", "Canines1981"] )
                    , ( "view", []                         )
                    , ( "save", []                         )
                    ] )

script103 = "pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result103 = Right ( Nothing
                  , [ ( "pull", ["Cats2016", "Dogs1977"]    )
                    , ( "name", ["Felines2018", "Canines1981"] )
                    , ( "view", []                         )
                    , ( "save", []                         )
                    ] )

script104 = "in plants.bib, in animals.bib, pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result104 = Right ( Just "plants.bib"
                  , [ ( "in",   ["animals.bib"]                )
                    , ( "pull", ["Cats2016", "Dogs1977"]       )
                    , ( "name", ["Felines2018", "Canines1981"] )
                    , ( "view", []                             )
                    , ( "save", []                             )
                    ] )

---------------------------------------------------------------------
-- Variations with special tokens

script201 = "in animals.bib, and and pull Cats2016 \n Felines2018 and \n view"
result201 = Right ( Just "animals.bib"
                  , [ ( "pull", ["Cats2016", "Felines2018"] )
                    , ( "view", []                          )
                    , ( "save", []                          )
                    ] )

script202 = "and in animals.bib,,,, and,and pull Cats2016 and\n\n name Felines2018 and \n view and and"
result202 = Right ( Just "animals.bib"
                  , [ ( "pull", ["Cats2016"]    )
                    , ( "name", ["Felines2018"] )
                    , ( "view", []              )
                    , ( "save", []              )
                    ] )

script203 = "in animals.bib, and and pull Cats2016 \n\n Dogs1977 \n, name Felines2018+ \n Canines1981++ and \n view"
result203 = Right ( Just "animals.bib"
                  , [ ( "pull", ["Cats2016", "Dogs1977"]          )
                    , ( "name", ["Felines2018+", "Canines1981++"] )
                    , ( "view", []                                )
                    , ( "save", []                                )
                    ] )

script204 = ",  \t   in animals.bib, and and pull Cats2016+ Dogs1977 \n, name Felines2018 \n +Canines1981 and \n view"
result204 = Right ( Just "animals.bib"
                  , [ ( "pull", ["Cats2016+", "Dogs1977"]       )
                    , ( "name", ["Felines2018", "+Canines1981"] )
                    , ( "view", []                              )
                    , ( "save", []                              )
                    ] )

script205 = ",      and      \n and and   ,,,,and   \t   \n\t\n    , and and, and"
result205 = Right ( Nothing
                  , [ ( "save", [] )
                    ] )

script206 = Tx.unlines $
    [ "in animals.bib,"
    , "    doi 10.1021/bi00685a029"
    , "        10.1021/j150544a010"
    , "        10.1021/bi00289a003,"
    , "    info This is what everything looks like after the download"
    , "and    name Cleland1975"
    , "            King1956"
    , "            Ray1983     ,"
    , "    edit vim"
    , ",   info This is what everything looks like before saving"
    ]
result206 = Right ( Just "animals.bib"
                  , [ ( "doi", [ "10.1021/bi00685a029"
                               , "10.1021/j150544a010"
                               , "10.1021/bi00289a003" ] )
                    , ( "info", [ "This", "is", "what", "everything", "looks"
                                , "like", "after", "the", "download" ] )
                    , ( "name", [ "Cleland1975"
                                , "King1956"
                                , "Ray1983" ] )
                    , ( "edit", [ "vim" ] )
                    , ( "info", [ "This", "is", "what", "everything"
                                , "looks", "like", "before", "saving" ] )
                    , ( "save", [] )
                    ] )

---------------------------------------------------------------------
-- These scripts test quoted strings

script301 = "get Cats2016, find \"Synthesis of 4a$\\alpha\" and view"
result301 = Right ( Nothing
                  , [ ( "get",  ["Cats2016"]                )
                    , ( "find", ["Synthesis of 4a$\\alpha"] )
                    , ( "view", []                          )
                    , ( "save", []                          )
                    ] )

script302 = "get Cats2016, find \"Synthesis of 4'a$\\alpha\" and view"
result302 = Right ( Nothing
                  , [ ( "get",  ["Cats2016"]                )
                    , ( "find", ["Synthesis of 4'a$\\alpha"] )
                    , ( "view", []                          )
                    , ( "save", []                          )
                    ] )

script303 = "get Cats2016, find \"Synthesis of 4\\\"a$\\alpha\" and view"
result303 = Right ( Nothing
                  , [ ( "get",  ["Cats2016"]                )
                    , ( "find", ["Synthesis of 4\"a$\\alpha"] )
                    , ( "view", []                          )
                    , ( "save", []                          )
                    ] )

script304 = "get Cats2016, find 'Synthesis of 4a$\\alpha' and view"
result304 = Right ( Nothing
                  , [ ( "get", ["Cats2016"]                 )
                    , ( "find", ["Synthesis of 4a$\\alpha"] )
                    , ( "view", []                          )
                    , ( "save", []                          )
                    ] )

script305 = "get Cats2016, find 'Synthesis \"of\" 4a$\\alpha' and view"
result305 = Right ( Nothing
                  , [ ( "get", ["Cats2016"]                     )
                    , ( "find", ["Synthesis \"of\" 4a$\\alpha"] )
                    , ( "view", []                              )
                    , ( "save", []                              )
                    ] )

script306 = "get Cats2016, find 'Synthesis \\\'of\\\' 4a$\\alpha' and view"
result306 = Right ( Nothing
                  , [ ( "get", ["Cats2016"]                     )
                    , ( "find", ["Synthesis 'of' 4a$\\alpha"] )
                    , ( "view", []                              )
                    , ( "save", []                              )
                    ] )

script307 = "get Cats2016, find 'Synthesis of 4a$\\alpha' 'cats like fish' and view"
result307 = Right ( Nothing
                  , [ ( "get", ["Cats2016"]                     )
                    , ( "find", [ "Synthesis of 4a$\\alpha"
                                , "cats like fish" ]            )
                    , ( "view", []                              )
                    , ( "save", []                              )
                    ] )

script308 = "\"\" ''get Cats2016 '  ', find '' 'cats like fish' and'''' view '' ''"
result308 = Right ( Nothing
                  , [ ( "get",  [ "Cats2016", "  " ]            )
                    , ( "find", [ "cats like fish" ]            )
                    , ( "view", []                              )
                    , ( "save", []                              )
                    ] )

---------------------------------------------------------------------
-- These scripts will fail to parse due to incorrect use of the
-- initial <in> command.

-- No argument for <in>
script401 = "in, pull Cats2016, name Felines2018 and view"
-- Too many arguments for <in>
script402 = "in animals.bib plants.bib, pull Cats2016, name Felines2018 and view"
-- Doubling of <in> handled the same as too many arguments for <in>
script403 = "in in animals.bib, pull Cats2016, name Felines2018 and view"
-- Quotes are not all closed
script404 = "get Cats2016, find 'Synthesis of 4a$\\alpha' 'cats like fish and view"
