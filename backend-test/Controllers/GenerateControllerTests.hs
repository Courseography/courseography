{-|
Description: Course Controller module tests.

Module that contains the tests for the functions in the Course Controller module.

-}

module Controllers.CourseControllerTests
( test_generateController
) where

import Config (runDb)
import Control.Monad (unless)
import Controllers.Generate (findAndSavePrereqsResponse)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist.Sqlite (insert_)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import TestHelpers (clearDatabase, withDatabase)

-- | List of test cases as (input course, course data, expected # of nodes in prereq graph)
findAndSavePrereqsResponseTestCases :: [(T.Text, Map.Map T.Text T.Text, Integer)]
findAndSavePrereqsResponseTestCases =
    [("CSC148",
    Map.fromList [
        ("name", "CSC148H1"),
        ("title", "Introduction to Computer Science"),
        ("description", "Abstract data types and data structures for implementing them. Linked data structures. Encapsulation and information-hiding. Object-oriented programming. Specifications. Analyzing the efficiency of programs. Recursion. This course assumes programming experience as provided by CSC108H1. Students who already have this background may consult the Computer Science Undergraduate Office for advice about skipping CSC108H1. Practical (P) sections consist of supervised work in the computing laboratory. These sections are offered when facilities are available, and attendance is required. Note: Students may request to move from CSC148H1 to CSC108H1 after the last day to add classes and before a deadline set by the course instructors, if space is available in CSC108H1 at the time of the request."),
        ("prereqs", "CSC108H1"),
        ("exclusions", "CSC111H1/  CSC207H1/  CSC148H5/  CSCA48H3/  CSCB07H3"),
        ("breadth", "The Physical and Mathematical Universes (5)"),
        ("distribution", "null"),
        ("prereqString", "CSC108H1"),
        ("coreqs", "null"),
        ("videoUrls", "null")
        ],
    2
    )]

-- | Run a test case (input course, expected # of nodes) on the findAndSavePrereqsResponse function.
runfindAndSavePrereqsResponseTest :: T.Text -> Map.Map T.Text T.Text -> Integer -> TestTree
runfindAndSavePrereqsResponseTest course courseData expected =
    testCase course $ do
        let currCourseName = fromMaybe "" $ Map.lookup "name" courseData

        let courseToInsert =
                Courses
                    { coursesCode = currCourseName
                    , coursesTitle = Map.lookup "title" courseData
                    , coursesDescription = Map.lookup "description" courseData
                    , coursesPrereqs = Map.lookup "prereqs" courseData
                    , coursesExclusions = Map.lookup "exclusions" courseData
                    , coursesBreadth = Map.lookup "breadth" courseData
                    , coursesDistribution = Nothing
                    , coursesPrereqString = Map.lookup "prereqString" courseData
                    , coursesCoreqs = Nothing
                    , coursesVideoUrls = Nothing
                    }

        runDb $ do
            clearDatabase
            unless (T.null currCourseName) $
                insert_ courseToInsert

        -- Run the function and check that generated graph only has the expected number of nodes.
        -- response <- runServerPartWithGraphGenerate Controllers.Generate.findAndSavePrereqsResponse (payload)

        -- TODO: take the response and extract the number of nodes within the generated graph, then assert that it is equal to the expected value.

        -- let actual =
        -- assertEqual ("Unexpected response for " ++ label) expected actual

-- | Run all the findAndSavePrereqsResponse test cases
runfindAndSavePrereqsResponseTests :: [TestTree]
runfindAndSavePrereqsResponseTests = map (\(course, courseData, expectedNodes) -> runfindAndSavePrereqsResponseTest course courseData expectedNodes) findAndSavePrereqsResponseTestCases

-- | Test suite for Generate Controller Module
test_generateController :: TestTree
test_generateController = withDatabase "Generate Controller tests" runfindAndSavePrereqsResponseTests
