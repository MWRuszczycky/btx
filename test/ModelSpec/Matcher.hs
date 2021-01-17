{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the 'matcher' test-suite for testing the Matcher module
-- =============================================================== --

import qualified Model.Types   as T
import qualified Data.Text     as Tx
import qualified Model.Matcher as M
import           Model.Matcher       ( (=~)             )
import           Data.Text           ( Text             )
import           Test.Hspec          ( Spec (..)
                                     , describe
                                     , hspec
                                     , it
                                     , shouldBe
                                     , shouldNotSatisfy
                                     , shouldSatisfy    )

main :: IO ()
main = hspec $ do
    describe "Tests for the very basic \"regex\"-like matcher" $ do
        simpleMatches
        simpleNonMatches
        constrainedMatches
        constrainedMismatches
        kleeneMatches
        infixMatches

simpleMatches :: Spec
simpleMatches = do
    it "satisfies one-to-one prefix character matches" $ do
        "cats"       `shouldSatisfy` ("cats" =~)
        "dogs"       `shouldSatisfy` ("dogs" =~)
        "dragons"    `shouldSatisfy` ("drag" =~)
        " 1 chicken" `shouldSatisfy` (" 1 chic" =~)
        "   indent"  `shouldSatisfy` ("   " =~)

simpleNonMatches :: Spec
simpleNonMatches = do
    it "identifies one-to-one prefix character mismatches" $ do
        ""           `shouldNotSatisfy` ("cats" =~)
        "cats"       `shouldNotSatisfy` ("" =~)
        ""           `shouldNotSatisfy` ("" =~)
        "cat s"      `shouldNotSatisfy` ("cats" =~)
        "dags"       `shouldNotSatisfy` ("dogs" =~)
        "dargons"    `shouldNotSatisfy` ("drag" =~)
        " 1 chicken" `shouldNotSatisfy` ("1 chic" =~)
        "   indent"  `shouldNotSatisfy` ("    " =~)

constrainedMatches :: Spec
constrainedMatches = do
    it "satisfies constrained prefix matches" $ do
        "cats"      `shouldSatisfy` ("ca.s" =~)
        "cats"      `shouldSatisfy` ("...s" =~)
        "cats"      `shouldSatisfy` ("ca\\ws" =~)
        "ca3s"      `shouldSatisfy` ("ca\\ws" =~)
        "ca0s"      `shouldSatisfy` ("ca\\ds" =~)
        "ca3s"      `shouldSatisfy` ("ca\\ds" =~)
        "cats"      `shouldSatisfy` ("ca\\Ds" =~)
        "ca s"      `shouldSatisfy` ("ca\\ss" =~)
        " cats"     `shouldSatisfy` ("\\sca\\ws" =~)
        "ca@*ts "   `shouldSatisfy` ("ca\\W\\Wt\\w" =~)
        "ca@*ts "   `shouldSatisfy` ("ca\\S\\Wt\\w\\s" =~)
        "ca*s"      `shouldSatisfy` ("ca\\*s" =~)
        "ca.s"      `shouldSatisfy` ("ca\\.s" =~)
        "\\alpha"   `shouldSatisfy` ("\\\\alpha" =~)

constrainedMismatches :: Spec
constrainedMismatches = do
    it "identifies constrained prefix mismatches" $ do
        ""          `shouldNotSatisfy` ("\\" =~)
        " "         `shouldNotSatisfy` ("\\" =~)
        "a"         `shouldNotSatisfy` ("\\" =~)
        "\\alpha"   `shouldNotSatisfy` ("alpha" =~)
        "alpha"     `shouldNotSatisfy` ("\\alpha" =~)
        "\\alpha"   `shouldNotSatisfy` ("\\alpha" =~)
        ""          `shouldNotSatisfy` ("." =~)
        "\n"        `shouldNotSatisfy` ("." =~)
        "cts"       `shouldNotSatisfy` ("c.ts" =~)
        "cats"      `shouldNotSatisfy` ("ca\\Ws" =~)
        "ca3s"      `shouldNotSatisfy` ("ca\\Ws" =~)
        "ca0s"      `shouldNotSatisfy` ("ca\\Ds" =~)
        "ca3s"      `shouldNotSatisfy` ("ca\\Ds" =~)
        "cats"      `shouldNotSatisfy` ("ca\\ds" =~)
        "ca s"      `shouldNotSatisfy` ("ca\\Ss" =~)
        " cats"     `shouldNotSatisfy` ("\\wca\\ws" =~)
        "ca@*ts "   `shouldNotSatisfy` ("ca\\W\\Wt\\w\\S" =~)
        "ca@*ts "   `shouldNotSatisfy` ("ca\\S\\Wt\\w\\w" =~)

kleeneMatches :: Spec
kleeneMatches = do
    it "satisfies Kleene-Star prefix matches" $ do
        "caaats"    `shouldSatisfy` ("ca*ts" =~)
        "caaats"    `shouldSatisfy` ("ca**ts" =~)
        "caaats"    `shouldSatisfy` ("ca***ts" =~)
        "cts"       `shouldSatisfy` ("ca*ts" =~)
        "cts"       `shouldSatisfy` ("ca**ts" =~)
        "c333ts"    `shouldSatisfy` ("c\\d*" =~)
        "cts"       `shouldSatisfy` ("c\\d*" =~)
        "c    ts"   `shouldSatisfy` ("c\\s*ts" =~)
        "c    ts"   `shouldSatisfy` ("c *ts" =~)
        "caatts"    `shouldSatisfy` ("ca*t*s" =~)
        "cats"      `shouldSatisfy` ("*cats" =~)
        "caatts"    `shouldSatisfy` ("c.*s" =~)
    it "identifies Kleene-Star prefix mismatches" $ do
        "caaaas"    `shouldNotSatisfy` ("ca*ts" =~)
        "cbbbts"    `shouldNotSatisfy` ("c\\d*ts" =~)
        ""          `shouldNotSatisfy` ("*" =~)
        "cc"        `shouldNotSatisfy` ("*" =~)
        "cc"        `shouldNotSatisfy` ("**" =~)
        "caaaatss"  `shouldNotSatisfy` ("c.*x" =~)

infixMatches :: Spec
infixMatches = do
    it "correctly finds infix matches" $ do
        "The are lots of cats in the basement where the elephant sleeps."
            `shouldSatisfy` (M.hasMatch "cats")
        "The are lots of cats in the basement where the elephant sleeps."
            `shouldSatisfy` (M.hasMatch "c\\w\\ws")
    it "correctly fails to find infix matches that do not exist" $ do
        "The are lots of cats in the basement where the elephant sleeps."
            `shouldNotSatisfy` (M.hasMatch "")
        "The are lots of cats in the basement where the elephant sleeps."
            `shouldNotSatisfy` (M.hasMatch "\\")
        "The are lots of cats in the basement where the elephant sleeps."
            `shouldNotSatisfy` (M.hasMatch "elephants")
        "The are lots of cats in the basement where the elephant sleeps."
            `shouldNotSatisfy` (M.hasMatch "c\\w\\ds")
