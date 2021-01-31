{-# LANGUAGE OverloadedStrings #-}

-- Do not add a module declaration or it will fail to compile

-- =============================================================== --
-- This is the 'mock' test-suite
-- It simulates the running of scripts on a test bibliography.
-- =============================================================== --

import qualified Controller.Controller as C
import qualified Model.Parsers.Config  as P
import qualified Model.Core.Types      as T
import qualified Data.Text             as Tx
import qualified Data.Text.IO          as Tx
import qualified View.View             as V
import           Data.Text                   ( Text, pack      )
import           Test.Hspec                  ( Spec      (..)
                                             , around_
                                             , describe
                                             , hspec
                                             , it
                                             , shouldBe        )
import           Control.Exception           ( bracket_        )
import           Control.Monad.Except        ( runExceptT      )
import           System.Directory            ( copyFile
                                             , listDirectory
                                             , removeFile      )

-- =============================================================== --
-- Tests involve a script/BibTeX-file pair. The script when run on
-- the test bibliography 'test/Mock/bib/testBib.bib' should generate
-- the associated BibTeX file(s) as the target. Scripts are listed by
-- name in the test/Mock/scripts.txt file.

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
    describe "btx manages error states correctly" $ do
        testScriptError "fails on invalid commands"
                        "script-bad-command"
        testScriptError "will not allow import and export bibs to be the same"
                        "script-same-import-export"
        testScriptError "will not allow export and import bibs to be the same"
                        "script-same-export-import"
        testScriptError "will not allow take before from"
                        "script-take-from"
        testScriptError "will not allow send before to"
                        "script-send-before-to"

-- =============================================================== --
-- Mocking the executable

mock :: String -> IO (Either String Text)
mock argStr = runExceptT $ mockConfig (words argStr)
                           >>= C.runBtx
                           >>= pure . (<> "\n")

mockConfig :: [String] -> T.ErrMonad T.Config
mockConfig args = do
    config <- C.configureBtx args
    pure $ config { T.cStyles = V.noStyles }

-- =============================================================== --
-- Utilities for preparing tests, running them and cleaning up after

---------------------------------------------------------------------
-- Testing scripts involving only a single bibliography

manageScriptTests :: IO () -> IO ()
-- ^Performs setup and teardown for a single-bibliography test.
manageScriptTests = bracket_ setupBib tearDown
    where setupBib = do copyFile "test/Mock/bib/testBib.bib"
                                 "test/Mock/testBib.bib"

testScriptOutputLog :: String -> String -> FilePath -> Spec
testScriptOutputLog cue name target = it (makeTitle cue name) action
    where action = do result      <- getTestScript name >>= mock
                      expectedLog <- Tx.readFile $ "test/Mock/testLogs/" <> target
                      case result of
                           Left  _         -> error "Script fails to execute!"
                           Right actualLog -> actualLog `shouldBe` expectedLog

testOneBibScript :: String -> String -> FilePath -> Spec
-- ^Run tests involving only a single bibliography.
testOneBibScript cue name target = it (makeTitle cue name) action
    where action = do getTestScript name >>= mock
                      expected <- readFile $ "test/Mock/bib/" <> target
                      actual   <- readFile   "test/Mock/testBib.bib"
                      actual `shouldBe` expected

testExportScript :: String -> String -> FilePath -> FilePath -> Spec
-- ^Run tests involving an export bibliography.
testExportScript cue name targetWk targetEx = it (makeTitle cue name) action
    where action = do getTestScript name >>= mock
                      expectedEx <- readFile $ "test/Mock/bib/" <> targetEx
                      expectedWk <- readFile $ "test/Mock/bib/" <> targetWk
                      actualEx   <- readFile "test/Mock/testExportBib.bib"
                      actualWk   <- readFile "test/Mock/testBib.bib"
                      actualEx `shouldBe` expectedEx
                      actualWk `shouldBe` expectedWk

testScriptError :: String -> String -> Spec
-- ^When an error occurs, the original bibliography should be unchanged.
testScriptError cue name = it (makeTitle cue name) action
    where action = do let errMsg = "Invalid script executes without error!"
                          path0  = "test/Mock/bib/testBib.bib"
                          path1  = "test/Mock/testBib.bib"
                      result <- mock =<< getTestScript name
                      case result of
                           Right _ -> error errMsg
                           Left  _ -> do expected <- readFile path0
                                         actual   <- readFile path1
                                         expected `shouldBe` actual

getTestScript :: String -> IO String
-- ^Finds the test script by name from the /test/Mock/scripts.txt file.
getTestScript name = do
    let scriptFile = "test/Mock/scripts.txt"
    scripts <- map ( break (== ':') ) . lines <$> readFile scriptFile
    case lookup name scripts of
         Nothing -> error $ "Cannot find test script: " <> name
         Just x  -> pure . tail $ x

makeTitle :: String -> String -> String
makeTitle cue script = cue <> "\n    script: " <> script

---------------------------------------------------------------------
-- Utilities for cleaning up the test bibliographies

tearDown :: IO ()
tearDown = do
    bibFiles <- filter isBibFile <$> listDirectory "test/Mock"
    mapM_ ( removeFile . ("test/Mock/" <>) ) bibFiles

isBibFile :: FilePath -> Bool
isBibFile = (== ".bib") . reverse . take 4 . reverse
