module Main where

-- =============================================================== --
-- This is the 'basic' test-suite
-- =============================================================== --

import qualified Model.Core.Types        as T
import qualified Controller              as C
import qualified Model.Core.ScriptParser as M
import Test.Hspec                             ( Spec      (..)
                                              , around_
                                              , describe
                                              , hspec
                                              , it
                                              , shouldBe        )
import Control.Exception                      ( bracket_        )
import Control.Monad.Except                   ( runExceptT      )
import System.Directory                       ( copyFile
                                              , listDirectory
                                              , removeFile      )

-- =============================================================== --
-- Running the basic test-suite
-- Tests involve a script/BibTeX-file pair. The script when run on
-- the test bibliography 'test/testBib.bib' should generate the
-- associated BibTeX file as the target. The scripts are found in
-- test/scripts and the target bibliographies are found in test/bibs

main :: IO ()
main = hspec $ around_ manageOneBibTest $ do
    describe "btx working with a single bibliography" $ do
        testOneBibScript "can read and format a badly formatted bibliography"
                         "script-formatted.txt"
                         "testBib-formatted.bib"
        testOneBibScript "can rename entries"
                         "script-renaming.txt"
                         "testBib-renaming.bib"
        testOneBibScript "can download entries by doi and rename"
                         "script-doi.txt"
                         "testBib-doi.bib"

-- =============================================================== --
-- Mocking the executable

mock :: [String] -> IO ()
-- ^Mocks the main function for executing scripts.
mock args = do
    script <- C.getScript args
    case M.parse script of
         T.Usage msg    -> return ()
         T.Help cs      -> return ()
         T.Normal fp cs -> runExceptT ( C.initBtx cs fp >>= C.runBtx )
                           >>= C.finish

-- =============================================================== --
-- Utilities for preparing tests, running them and cleaning up after

---------------------------------------------------------------------
-- Testing scripts involving only a single bibliography

manageOneBibTest :: IO () -> IO ()
-- ^Performs setup and teardown for a single-bibliography test.
manageOneBibTest = bracket_ setupBib tearDown
    where setupBib = copyFile "test/bib/testBib.bib" "test/testBib.bib"

testOneBibScript :: String -> FilePath -> FilePath -> Spec
-- ^Runs a test script on a single-bibliography test. The file name
-- of the script and the target bibliography are provided as
-- arguments. The processed  bibliography should end up being the
-- same as the target bibliography.
testOneBibScript cue script target = it (makeTitle cue script) action
    where action = do readFile ("test/scripts/" ++ script) >>= mock . words
                      expected <- readFile $ "test/bib/" ++ target
                      actual   <- readFile $ "test/testBib.bib"
                      actual `shouldBe` expected

makeTitle :: String -> String -> String
makeTitle cue script = cue ++ "\n    Script file: " ++ script

---------------------------------------------------------------------
-- Utilities for cleaning up the test bibliographies

tearDown :: IO ()
tearDown = do
    bibFiles <- filter isBibFile <$> listDirectory "test"
    mapM_ ( removeFile . ("test/" ++) ) bibFiles

isBibFile :: FilePath -> Bool
isBibFile fp = (== ".bib") . reverse . take 4 . reverse $ fp
