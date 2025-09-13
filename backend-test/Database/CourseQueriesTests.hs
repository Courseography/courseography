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
import Models.Post (reqsForPost)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, withDatabase)

-- | List of test cases as (label, requirements to insert, input program, expected output)
reqsForPostTestCases :: [(String, T.Text, T.Text, String)]
reqsForPostTestCases =
    [ ("No program", "", "", "[]")
    , ("Valid program", "/CSC199H1/", "ASMAJ1689", "[\"CSC199H1\"]")
    , ("Invalid program", "", "ABCDE1234", "[]")
    ]

-- | Run a test case (case, requirements, input, expected output) on the reqsForPost function.
runReqsForPostTest :: String -> T.Text -> T.Text -> String -> TestTree
runReqsForPostTest label reqsToInsert program expected =
    testCase label $ do
        currentTime <- liftIO getCurrentTime
        let testPost = Post Major "Computer Science" program "Sample post description" reqsToInsert currentTime currentTime

        runDb $ do
            clearDatabase
            insert_ testPost

        let requirements = reqsForPost testPost
        let actual = show requirements
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the reqsForPost test cases
runReqsForPostTests :: [TestTree]
runReqsForPostTests = map (\(label, reqsToInsert, program, expected) -> runReqsForPostTest label reqsToInsert program expected) reqsForPostTestCases

-- | Test suite for CourseQueries Module
test_courseQueries :: TestTree
test_courseQueries =
    withDatabase "Course Queries tests" runReqsForPostTests
