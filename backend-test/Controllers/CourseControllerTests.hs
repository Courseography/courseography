{-|
Description: Course Controller module tests.

Module that contains the tests for the functions in the Course Controller module.

-}

module Controllers.CourseControllerTests
( courseControllerTestSuite
) where

import Config (runDb)
import Controllers.Course (index, retrieveCourse)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Courses(..))
import Happstack.Server (rsBody)
import Test.HUnit (Test(..), assertEqual)
import TestHelpers (clearDatabase, runServerPart, runServerPartWithQuery)

-- | Helper function to insert courses into the database
insertCourses :: [T.Text] -> SqlPersistM ()
insertCourses = mapM_ insertCourse
    where
        insertCourse code = insert_ (Courses code Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [])

-- | List of test cases as (input courses, label, expected output)
indexTestCases :: [(String, [T.Text], String)]
indexTestCases =
    [ ("Empty database", [], "")
    , ("One course", ["CSC199"], "CSC199\n")
    , ("Multiple courses", ["CSC101", "CSC102", "CSC103", "CSC104", "CSC105"],
       "CSC101\nCSC102\nCSC103\nCSC104\nCSC105\n")
    ]

-- | List of test cases as (input courses, course name, expected JSON output)
retrieveCourseTestCases :: [(String, [T.Text], String, String)]
retrieveCourseTestCases =
    [ ("Course exists", ["CSC199"], "CSC199", "{\"allMeetingTimes\":[],\"breadth\":null,\"coreqs\":null,\"description\":null,\"distribution\":null,\"exclusions\":null,\"name\":\"CSC199\",\"prereqString\":null,\"title\":null,\"videoUrls\":[]}")
    , ("Course does not exist", [], "CSC999", "null")
    , ("No course provided", [], "", "null")
    ]

-- | Run a test case (case, input, expected output) on the index function.
runIndexTest :: String -> [T.Text] -> String -> Test
runIndexTest label courses expected =
    TestLabel label $ TestCase $ do
        runDb $ do
            clearDatabase
            insertCourses courses
        response <- runServerPart Controllers.Course.index
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run a test case (case, input, expected output) on the retrieveCourse function.
runRetrieveCourseTest :: String -> [T.Text] -> String -> String -> Test
runRetrieveCourseTest label courses name expected =
    TestLabel label $ TestCase $ do
        runDb $ do
            clearDatabase
            insertCourses courses
        response <- runServerPartWithQuery Controllers.Course.retrieveCourse name
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the index test cases
runIndexTests :: [Test]
runIndexTests = map (\(label, courses, expected) -> runIndexTest label courses expected) indexTestCases

-- | Run all the retrieveCourse test cases
runRetrieveCourseTests :: [Test]
runRetrieveCourseTests = map (\(label, courses, name, expected) -> runRetrieveCourseTest label courses name expected) retrieveCourseTestCases

-- | Test suite for Course Controller Module
courseControllerTestSuite :: Test
courseControllerTestSuite = TestLabel "Course Controller tests" $ TestList (runIndexTests ++ runRetrieveCourseTests)