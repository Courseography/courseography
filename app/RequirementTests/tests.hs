{-|
Description: Main module for testing.

Module that acts as interface for testing multiple test suites using cabal.

-}

module Main
(  main  ) where

import Control.Monad
import Config (databasePath)
import Data.Text (unpack)
import Database.Database(setupDatabase)
import RequirementTests.ModifierTests (modifierTestSuite)
import RequirementTests.PostParserTests (postTestSuite)
import RequirementTests.PreProcessingTests (preProcTestSuite)
import RequirementTests.ReqParserTests (reqTestSuite)
import RequirementTests.CourseControllerTests (courseContTestSuite)
import System.Directory (removeFile)
import System.Environment (setEnv, unsetEnv)
import qualified System.Exit as Exit
import Test.HUnit (Test (..), failures, runTestTT)

-- Single test encompassing all test suites
tests :: IO Test
tests = do
    courseTest <- courseContTestSuite
    return $ TestList [reqTestSuite, postTestSuite, preProcTestSuite, modifierTestSuite, courseTest]

main :: IO ()
main = do
    setEnv "APP_ENV" "test"
    setupDatabase
    testSuites <- tests
    count <- runTestTT testSuites
    when (failures count > 0) Exit.exitFailure
    path <- databasePath
    removeFile $ unpack path
    unsetEnv "APP_ENV"
