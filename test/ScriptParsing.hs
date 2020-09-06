{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the 'parsing' test-suite for testing the                --
-- Model.Core.ScriptParser module
-- =============================================================== --

import qualified Data.Text               as Tx
import qualified Model.Core.Types        as T
import Model.Core.ScriptParser                 ( parse              )
import Data.Text                               ( Text               )
import Test.Hspec                              ( Spec (..)
                                               , describe
                                               , hspec
                                               , it
                                               , shouldBe
                                               , shouldSatisfy      )

main :: IO ()
main = hspec $ do
    describe "The btx script-parser" $ do
        it "handles simple scripts" $ do
            parse script101 `shouldBe` result101
            parse script102 `shouldBe` result102
            parse script103 `shouldBe` result103
            parse script104 `shouldBe` result104
        it "handles variations \\n and '+' correctly" $ do
            parse script201 `shouldBe` result201
            parse script202 `shouldBe` result202
            parse script203 `shouldBe` result203
            parse script204 `shouldBe` result204
            parse script205 `shouldBe` result205
            parse script206 `shouldBe` result206
        it "handles quoted strings correctly" $ do
            parse script301 `shouldBe` result301
            parse script302 `shouldBe` result302
            parse script303 `shouldBe` result303
            parse script304 `shouldBe` result304
            parse script305 `shouldBe` result305
            parse script306 `shouldBe` result306
            parse script307 `shouldBe` result307
            parse script308 `shouldBe` result308
        it "handles malformed scripts" $ do
            parse script401 `shouldSatisfy` parseFailure
            parse script402 `shouldSatisfy` parseFailure
            parse script403 `shouldSatisfy` parseFailure
            parse script404 `shouldSatisfy` parseFailure

---------------------------------------------------------------------
-- Helper functions

parseFailure :: T.Start -> Bool
parseFailure (T.Usage _) = True
parseFailure _           = False

-- =============================================================== --
-- Test scripts

---------------------------------------------------------------------
-- Simple scripts

script101 = "in animals.bib, pull Cats2016, name Felines2018 and view"
result101 = T.Script (Just "animals.bib")
                     [ ( "pull", ["Cats2016"]    )
                     , ( "name", ["Felines2018"] )
                     , ( "view", []              )
                     , ( "save", []              )
                     ]

script102 = "in animals.bib, pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result102 = T.Script (Just "animals.bib")
                     [ ( "pull", ["Cats2016", "Dogs1977"]    )
                     , ( "name", ["Felines2018", "Canines1981"] )
                     , ( "view", []                         )
                     , ( "save", []                         )
                     ]

script103 = "pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result103 = T.Script Nothing
                     [ ( "pull", ["Cats2016", "Dogs1977"]    )
                     , ( "name", ["Felines2018", "Canines1981"] )
                     , ( "view", []                         )
                     , ( "save", []                         )
                     ]

script104 = "in plants.bib, in animals.bib, pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result104 = T.Script (Just "plants.bib")
                     [ ( "in",   ["animals.bib"]                )
                     , ( "pull", ["Cats2016", "Dogs1977"]       )
                     , ( "name", ["Felines2018", "Canines1981"] )
                     , ( "view", []                             )
                     , ( "save", []                             )
                     ]

---------------------------------------------------------------------
-- Variations with special tokens

script201 = "in animals.bib, and and pull Cats2016 \n Felines2018 and \n view"
result201 = T.Script (Just "animals.bib")
                     [ ( "pull", ["Cats2016", "Felines2018"] )
                     , ( "view", []                          )
                     , ( "save", []                          )
                     ]

script202 = "and in animals.bib,,,, and,and pull Cats2016 and\n\n name Felines2018 and \n view and and"
result202 = T.Script (Just "animals.bib")
                     [ ( "pull", ["Cats2016"]    )
                     , ( "name", ["Felines2018"] )
                     , ( "view", []              )
                     , ( "save", []              )
                     ]

script203 = "in animals.bib, and and pull Cats2016 \n\n Dogs1977 \n, name Felines2018+ \n Canines1981++ and \n view"
result203 = T.Script (Just "animals.bib")
                     [ ( "pull", ["Cats2016", "Dogs1977"]          )
                     , ( "name", ["Felines2018+", "Canines1981++"] )
                     , ( "view", []                                )
                     , ( "save", []                                )
                     ]

script204 = ",  \t   in animals.bib, and and pull Cats2016+ Dogs1977 \n, name Felines2018 \n +Canines1981 and \n view"
result204 = T.Script (Just "animals.bib")
                     [ ( "pull", ["Cats2016+", "Dogs1977"]       )
                     , ( "name", ["Felines2018", "+Canines1981"] )
                     , ( "view", []                              )
                     , ( "save", []                              )
                     ]
script205 = ",      and      \n and and   ,,,,and   \t   \n\t\n    , and and, and"
result205 = T.Script Nothing
                     [ ( "save", [] )
                     ]

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
result206 = T.Script (Just "animals.bib")
                     [ ( "doi", [ "10.1021/bi00685a029"
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
                     ]

---------------------------------------------------------------------
-- These scripts test quoted strings

script301 = "get Cats2016, find \"Synthesis of 4a$\\alpha\" and view"
result301 = T.Script Nothing
                     [ ( "get",  ["Cats2016"]                )
                     , ( "find", ["Synthesis of 4a$\\alpha"] )
                     , ( "view", []                          )
                     , ( "save", []                          )
                     ]

script302 = "get Cats2016, find \"Synthesis of 4'a$\\alpha\" and view"
result302 = T.Script Nothing
                     [ ( "get",  ["Cats2016"]                )
                     , ( "find", ["Synthesis of 4'a$\\alpha"] )
                     , ( "view", []                          )
                     , ( "save", []                          )
                     ]

script303 = "get Cats2016, find \"Synthesis of 4\\\"a$\\alpha\" and view"
result303 = T.Script Nothing
                     [ ( "get",  ["Cats2016"]                )
                     , ( "find", ["Synthesis of 4\"a$\\alpha"] )
                     , ( "view", []                          )
                     , ( "save", []                          )
                     ]

script304 = "get Cats2016, find 'Synthesis of 4a$\\alpha' and view"
result304 = T.Script Nothing
                     [ ( "get", ["Cats2016"]                 )
                     , ( "find", ["Synthesis of 4a$\\alpha"] )
                     , ( "view", []                          )
                     , ( "save", []                          )
                     ]

script305 = "get Cats2016, find 'Synthesis \"of\" 4a$\\alpha' and view"
result305 = T.Script Nothing
                     [ ( "get", ["Cats2016"]                     )
                     , ( "find", ["Synthesis \"of\" 4a$\\alpha"] )
                     , ( "view", []                              )
                     , ( "save", []                              )
                     ]

script306 = "get Cats2016, find 'Synthesis \\\'of\\\' 4a$\\alpha' and view"
result306 = T.Script Nothing
                     [ ( "get", ["Cats2016"]                     )
                     , ( "find", ["Synthesis 'of' 4a$\\alpha"] )
                     , ( "view", []                              )
                     , ( "save", []                              )
                     ]

script307 = "get Cats2016, find 'Synthesis of 4a$\\alpha' 'cats like fish' and view"
result307 = T.Script Nothing
                     [ ( "get", ["Cats2016"]                     )
                     , ( "find", [ "Synthesis of 4a$\\alpha"
                                 , "cats like fish" ]            )
                     , ( "view", []                              )
                     , ( "save", []                              )
                     ]

script308 = "\"\" ''get Cats2016 '  ', find '' 'cats like fish' and'''' view '' ''"
result308 = T.Script Nothing
                     [ ( "get",  [ "Cats2016", "  " ]            )
                     , ( "find", [ "cats like fish" ]            )
                     , ( "view", []                              )
                     , ( "save", []                              )
                     ]


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
