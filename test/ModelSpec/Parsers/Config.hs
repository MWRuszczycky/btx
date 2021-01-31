{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the 'parsing' test-suite for testing the                --
-- Model.Parsers.Scripts module
-- =============================================================== --

import qualified Model.Core.Types      as T
import qualified Data.Text             as Tx
import qualified Data.Text.IO          as Tx
import qualified Model.Parsers.Config  as P
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
    describe "Model.Parsers.Config.parseScript" $ do
        spec_parseScript
    describe "Model.Parsers.Config.parseConfigTxt" $ do
        spec_parseConfigTxt

-- =============================================================== --
-- Model.Parsers.Config.parseScript

spec_parseScript :: Spec
spec_parseScript = do
    it "handles simple scripts" $ do
        P.parseScript script101 `shouldBe` result101
        P.parseScript script102 `shouldBe` result102
        P.parseScript script103 `shouldBe` result103
        P.parseScript script104 `shouldBe` result104
    it "handles variations \\n and '+' correctly" $ do
        P.parseScript script201 `shouldBe` result201
        P.parseScript script202 `shouldBe` result202
        P.parseScript script203 `shouldBe` result203
        P.parseScript script204 `shouldBe` result204
        P.parseScript script205 `shouldBe` result205
        P.parseScript script206 `shouldBe` result206
    it "handles quoted strings correctly" $ do
        P.parseScript script301 `shouldBe` result301
        P.parseScript script302 `shouldBe` result302
        P.parseScript script303 `shouldBe` result303
        P.parseScript script304 `shouldBe` result304
        P.parseScript script305 `shouldBe` result305
        P.parseScript script306 `shouldBe` result306
        P.parseScript script307 `shouldBe` result307
        P.parseScript script308 `shouldBe` result308
    it "handles malformed scripts" $ do
        P.parseScript script401 `shouldSatisfy` isLeft
        P.parseScript script402 `shouldSatisfy` isLeft
        P.parseScript script403 `shouldSatisfy` isLeft
        P.parseScript script404 `shouldSatisfy` isLeft

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

-- =============================================================== -- 
-- Model.Parsers.Config.parseConfigTxt

spec_parseConfigTxt :: Spec
spec_parseConfigTxt = do
    it "works with mock_config_1" $ do
        testFileTxt <- Tx.readFile "test/Mock/configs/mock_config_1"
        P.parseConfigTxt testFileTxt `shouldBe`
            Right [ ( ("key" <>) . Tx.pack . show $ n , "value" )
                    | n <- [1..20] ]
    it "works with mock_config_2" $ do
        testFileTxt <- Tx.readFile "test/Mock/configs/mock_config_2"
        P.parseConfigTxt testFileTxt `shouldBe`
            Right [ ( ("key" <>) . Tx.pack . show $ n, "value value value" )
                    | n <- [1..10] ]
    it "works with mock_config_3" $ do
        testFileTxt <- Tx.readFile "test/Mock/configs/mock_config_3"
        P.parseConfigTxt testFileTxt `shouldBe`
            Right [ ( ("key" <>) . Tx.pack . show $ n, "value" )
                    | n <- [1..3] ]
    it "works with mock_config_4" $ do
        testFileTxt <- Tx.readFile "test/Mock/configs/mock_config_4"
        P.parseConfigTxt testFileTxt `shouldBe`
            Right [ ( ("key" <>) . Tx.pack . show $ n, "value" )
                    | n <- [1..5] ]
