{-|
Description: CourseQueries module tests.

Module that contains the tests for the functions in the CourseQueries module.

-}

module Database.CourseQueriesTests
( test_courseQueries
) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.DataType (PostType (..))
import Database.Persist.Sqlite (insert_)
import Database.Tables (Post (..))
import Models.Program (reqsForProgram)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, withDatabase)

-- | List of test cases as (label, requirements to insert, input program, expected output)
reqsForProgramTestCases :: [(String, T.Text, T.Text, String)]
reqsForProgramTestCases =
    [ ("No program", "", "", "[]")
    , ("Valid program", "/CSC199H1/", "ASMAJ1689", "[\"CSC199H1\"]")
    , ("Invalid program", "", "ABCDE1234", "[]")
    ]

-- | Run a test case (case, requirements, input, expected output) on the reqsForProgram function.
runReqsForProgramTest :: String -> T.Text -> T.Text -> String -> TestTree
runReqsForProgramTest label reqsToInsert program expected =
    testCase label $ do
        currentTime <- liftIO getCurrentTime
        let testProgram = Post Major "Computer Science" program "Sample post description" reqsToInsert currentTime currentTime

        runDb $ do
            clearDatabase
            insert_ testProgram

        let requirements = reqsForProgram testProgram
        let actual = show requirements
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the reqsForProgram test cases
runReqsForProgramTests :: [TestTree]
runReqsForProgramTests = map (\(label, reqsToInsert, program, expected) -> runReqsForProgramTest label reqsToInsert program expected) reqsForProgramTestCases

-- | Test suite for CourseQueries Module
test_courseQueries :: TestTree
test_courseQueries =
    withDatabase "Course Queries tests" runReqsForProgramTests
