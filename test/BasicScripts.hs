{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the 'basic' test-suite: BasicScripts.hs
-- =============================================================== --

import qualified Model.Core.Types        as T
import qualified Controller              as C
import qualified Model.Core.ScriptParser as M
import qualified Model.Core.Formatting   as F
import qualified Data.Text               as Tx
import qualified Data.Text.IO            as Tx
import Data.Text                                ( Text, pack      )
import Test.Hspec                               ( Spec      (..)
                                                , around_
                                                , describe
                                                , hspec
                                                , it
                                                , shouldBe        )
import Control.Exception                        ( bracket_        )
import Control.Monad.Except                     ( runExceptT      )
import System.Directory                         ( copyFile
                                                , listDirectory
                                                , removeFile      )

-- =============================================================== --
-- Running the basic test-suite
-- Tests involve a script/BibTeX-file pair. The script when run on
-- the test bibliography 'test/bib/testBib.bib' should generate the
-- associated BibTeX file(s) as the target. Scripts are listed by
-- name in the test/scripts.txt file.

main :: IO ()
main = hspec $ around_ manageScriptTests $ do
    describe "btx generates correct output logs" $ do
        testScriptOutputLog "generates correct info log"
                            "script-info"
                            "testLog-info.log"
        testScriptOutputLog "generates correct view log"
                            "script-view"
                            "testLog-view.log"
        testScriptOutputLog "generates correct view list log"
                            "script-list"
                            "testLog-list.log"
        testScriptOutputLog "generates correct view tex log"
                            "script-view-tex"
                            "testLog-view-tex.log"
    describe "btx works with a single bibliography" $ do
        testOneBibScript "can read and format a badly formatted bibliography"
                         "script-formatted"
                         "testBib-formatted.bib"
        testOneBibScript "can rename entries"
                         "script-renaming"
                         "testBib-renaming.bib"
        testOneBibScript "can download entries by doi and rename"
                         "script-doi"
                         "testBib-doi.bib"
        testOneBibScript "can find entries with a single expression"
                         "script-find1"
                         "testBib-find1.bib"
        testOneBibScript "can find entries with two expressions"
                         "script-find2"
                         "testBib-find2.bib"
    describe "btx works with an export bibliography" $ do
        testExportScript "'to export, send' syntax works"
                         "script-tosend"
                         "testBib-formatted.bib"
                         "testExportBib-renaming.bib"
        testExportScript "'send to export' syntax works"
                         "script-sendto"
                         "testBib-formatted.bib"
                         "testExportBib-renaming.bib"
        testExportScript "masked exports work"
                         "script-send-toss"
                         "testBib-formatted.bib"
                         "testExportBib-masked.bib"

-- =============================================================== --
-- Mocking the executable

mock :: [String] -> IO Text
-- ^Mocks the main function for executing scripts.
mock args = do
    script <- C.getScript args
    case M.parse script of
         T.Usage msg      -> pure . pack $ msg
         T.Help cs        -> pure . pack . unlines $ cs
         T.Script mbFp cs -> let startUp = C.initBtx F.noStyles mbFp
                             in  runExceptT ( startUp >>= C.runBtx cs )
                                 >>= mockFinish

mockFinish :: Either String T.BtxState -> IO Text
mockFinish (Left msg) = pure . pack $ msg ++ "\n"
mockFinish (Right bs) = pure . (<> "\n") . T.logger $ bs

-- =============================================================== --
-- Utilities for preparing tests, running them and cleaning up after

---------------------------------------------------------------------
-- Testing scripts involving only a single bibliography

manageScriptTests :: IO () -> IO ()
-- ^Performs setup and teardown for a single-bibliography test.
manageScriptTests = bracket_ setupBib tearDown
    where setupBib = copyFile "test/bib/testBib.bib" "test/testBib.bib"

testScriptOutputLog :: String -> String -> FilePath -> Spec
testScriptOutputLog cue name target = it (makeTitle cue name) action
    where action = do actualLog   <- getTestScript name >>= mock . words
                      expectedLog <- Tx.readFile $ "test/testLogs/" ++ target
                      actualLog `shouldBe` expectedLog

testOneBibScript :: String -> String -> FilePath -> Spec
-- ^Run tests involving only a single bibliography.
testOneBibScript cue name target = it (makeTitle cue name) action
    where action = do getTestScript name >>= mock . words
                      expected <- readFile $ "test/bib/" ++ target
                      actual   <- readFile "test/testBib.bib"
                      actual `shouldBe` expected

testExportScript :: String -> String -> FilePath -> FilePath -> Spec
-- ^Run tests involving an export bibliography.
testExportScript cue name targetWk targetEx = it (makeTitle cue name) action
    where action = do getTestScript name >>= mock . words
                      expectedEx <- readFile $ "test/bib/" ++ targetEx
                      expectedWk <- readFile $ "test/bib/" ++ targetWk
                      actualEx   <- readFile "test/testExportBib.bib"
                      actualWk   <- readFile "test/testBib.bib"
                      actualEx `shouldBe` expectedEx
                      actualWk `shouldBe` expectedWk

getTestScript :: String -> IO String
-- ^Finds the test script by name from the /test/scripts.txt file.
getTestScript name = do
    scripts <- map ( break (== ':') ) . lines <$> readFile "test/scripts.txt"
    case lookup name scripts of
         Nothing -> pure []
         Just x  -> pure . tail $ x

makeTitle :: String -> String -> String
makeTitle cue script = cue ++ "\n    script: " ++ script

---------------------------------------------------------------------
-- Utilities for cleaning up the test bibliographies

tearDown :: IO ()
tearDown = do
    bibFiles <- filter isBibFile <$> listDirectory "test"
    mapM_ ( removeFile . ("test/" ++) ) bibFiles

isBibFile :: FilePath -> Bool
isBibFile = (== ".bib") . reverse . take 4 . reverse
