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
            parse script1 `shouldBe` result1
            parse script2 `shouldBe` result2
            parse script3 `shouldBe` result3
            parse script4 `shouldBe` result4
        it "handles variations of and/with/,/+/\\n" $ do
            parse script5 `shouldBe` result5
            parse script6 `shouldBe` result6
            parse script7 `shouldBe` result7
            parse script8 `shouldBe` result8
            parse script9 `shouldBe` result9
        it "handles malformed scripts" $ do
            parse script10 `shouldSatisfy` parseFailure
            parse script11 `shouldSatisfy` parseFailure
            parse script12 `shouldSatisfy` parseFailure
        it "handles quoted strings correctly" $ do
            parse script13 `shouldBe` result13
            parse script14 `shouldBe` result14
            parse script15 `shouldBe` result15
            parse script16 `shouldBe` result16
            parse script17 `shouldBe` result17
            parse script18 `shouldBe` result18

---------------------------------------------------------------------
-- Helper functions

parseFailure :: T.Start -> Bool
parseFailure (T.Usage _) = True
parseFailure _           = False

---------------------------------------------------------------------
-- Test scripts

script1 = Right "in animals.bib, pull Cats2016, name Felines2018 and view"
result1 = T.Script (Just "animals.bib")
                   [ ( "pull", ["Cats2016"]    )
                   , ( "name", ["Felines2018"] )
                   , ( "view", []              )
                   , ( "save", []              )
                   ]

script2 = Right "in animals.bib, pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result2 = T.Script (Just "animals.bib")
                   [ ( "pull", ["Cats2016", "Dogs1977"]    )
                   , ( "name", ["Felines2018", "Canines1981"] )
                   , ( "view", []                         )
                   , ( "save", []                         )
                   ]

script3 = Right "pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result3 = T.Script Nothing
                   [ ( "pull", ["Cats2016", "Dogs1977"]    )
                   , ( "name", ["Felines2018", "Canines1981"] )
                   , ( "view", []                         )
                   , ( "save", []                         )
                   ]

script4 = Right "in plants.bib, in animals.bib, pull Cats2016 Dogs1977, name Felines2018 Canines1981, and view"
result4 = T.Script (Just "plants.bib")
                   [ ( "in",   ["animals.bib"]                )
                   , ( "pull", ["Cats2016", "Dogs1977"]       )
                   , ( "name", ["Felines2018", "Canines1981"] )
                   , ( "view", []                             )
                   , ( "save", []                             )
                   ]

script5 = Right "in animals.bib, and and pull Cats2016 \n name Felines2018 and \n view"
result5 = T.Script (Just "animals.bib")
                   [ ( "pull", ["Cats2016"]    )
                   , ( "name", ["Felines2018"] )
                   , ( "view", []              )
                   , ( "save", []              )
                   ]

script6 = Right "and in animals.bib,,,, and,and pull Cats2016 and\n\n name Felines2018 and \n view and and"
result6 = T.Script (Just "animals.bib")
                   [ ( "pull", ["Cats2016"]    )
                   , ( "name", ["Felines2018"] )
                   , ( "view", []              )
                   , ( "save", []              )
                   ]

script7 = Right "in animals.bib, and and pull Cats2016 and with Dogs1977 \n name Felines2018 \n + Canines1981 and \n view"
result7 = T.Script (Just "animals.bib")
                   [ ( "pull", ["Cats2016", "Dogs1977"]       )
                   , ( "name", ["Felines2018", "Canines1981"] )
                   , ( "view", []                             )
                   , ( "save", []                             )
                   ]

script8 = Right ", with in animals.bib, and with and pull Cats2016 + and with Dogs1977 \n name Felines2018 \n + Canines1981 and \n view"
result8 = T.Script (Just "animals.bib")
                   [ ( "pull", ["Cats2016", "Dogs1977"]       )
                   , ( "name", ["Felines2018", "Canines1981"] )
                   , ( "view", []                             )
                   , ( "save", []                             )
                   ]
script9 = Right ", with and with \n and and + ,,,,and with \n\n with and and, and"
result9 = T.Script Nothing
                   [ ( "save", [] )
                   ]

---------------------------------------------------------------------
-- These scripts will fail to parse due to incorrect use of the
-- initial <in> command.

script10 = Right "in, pull Cats2016, name Felines2018 and view"
script11 = Right "in animals.bib plants.bib, pull Cats2016, name Felines2018 and view"
script12 = Right "in in animals.bib, pull Cats2016, name Felines2018 and view"

---------------------------------------------------------------------
-- These scripts test quoted strings

script13 = Right "get Cats2016, find \"Synthesis of 4a$\\alpha\" and view"
result13 = T.Script Nothing
                   [ ( "get",  ["Cats2016"]                )
                   , ( "find", ["Synthesis of 4a$\\alpha"] )
                   , ( "view", []                          )
                   , ( "save", []                          )
                   ]

script14 = Right "get Cats2016, find \"Synthesis of 4'a$\\alpha\" and view"
result14 = T.Script Nothing
                   [ ( "get",  ["Cats2016"]                )
                   , ( "find", ["Synthesis of 4'a$\\alpha"] )
                   , ( "view", []                          )
                   , ( "save", []                          )
                   ]

script15 = Right "get Cats2016, find \"Synthesis of 4\\\"a$\\alpha\" and view"
result15 = T.Script Nothing
                   [ ( "get",  ["Cats2016"]                )
                   , ( "find", ["Synthesis of 4\"a$\\alpha"] )
                   , ( "view", []                          )
                   , ( "save", []                          )
                   ]

script16 = Right "get Cats2016, find 'Synthesis of 4a$\\alpha' and view"
result16 = T.Script Nothing
                   [ ( "get", ["Cats2016"]                 )
                   , ( "find", ["Synthesis of 4a$\\alpha"] )
                   , ( "view", []                          )
                   , ( "save", []                          )
                   ]

script17 = Right "get Cats2016, find 'Synthesis \"of\" 4a$\\alpha' and view"
result17 = T.Script Nothing
                   [ ( "get", ["Cats2016"]                     )
                   , ( "find", ["Synthesis \"of\" 4a$\\alpha"] )
                   , ( "view", []                              )
                   , ( "save", []                              )
                   ]

script18 = Right "get Cats2016, find 'Synthesis \\\'of\\\' 4a$\\alpha' and view"
result18 = T.Script Nothing
                   [ ( "get", ["Cats2016"]                     )
                   , ( "find", ["Synthesis 'of' 4a$\\alpha"] )
                   , ( "view", []                              )
                   , ( "save", []                              )
                   ]
