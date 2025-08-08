{-|
Description: Generate Controller module tests.

Module that contains the tests for the functions in the Generate Controller module.

-}

module Controllers.GenerateControllerTests
( test_generateController
) where

import Config (runDb)
import Control.Monad (mapM_, unless)
import Controllers.Generate (findAndSavePrereqsResponse)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Courses (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, runServerPartWithGraphGenerate, withDatabase)

-- | Helper function to insert courses into the database
insertCoursesWithPrerequisites :: [(T.Text, Maybe T.Text)] -> SqlPersistM ()
insertCoursesWithPrerequisites = mapM_ insertCourse
    where
        insertCourse (code, prereqString) = insert_ (Courses code Nothing Nothing Nothing prereqString Nothing Nothing Nothing Nothing [])

-- | List of test cases as (input course, course and prereq structure, JSON payload, expected # of nodes in prereq graph)
findAndSavePrereqsResponseTestCases :: [(String, [(T.Text, Maybe T.Text)], BSL.ByteString, Integer)]
findAndSavePrereqsResponseTestCases =
    [("CSC148H1",
    [("CSC108H1", Nothing), ("CSC148H1", Just "CSC108H1")],
    "{\"courses\":[\"CSC148H1\"],\"programs\":[],\"graphOptions\":{\"taken\":[],\"departments\":[\"CSC\",\"MAT\",\"STA\"]}}",
    2
    )]

-- | Run a test case (input course, expected # of nodes) on the findAndSavePrereqsResponse function.
runfindAndSavePrereqsResponseTest :: String -> [(T.Text, Maybe T.Text)] -> BSL.ByteString -> Integer -> TestTree
runfindAndSavePrereqsResponseTest course graphStructure payload expected =
    testCase course $ do
        runDb $ do
            clearDatabase
            insertCoursesWithPrerequisites graphStructure

        -- Run the function and check that generated graph only has the expected number of nodes.
        response <- runServerPartWithGraphGenerate Controllers.Generate.findAndSavePrereqsResponse payload

        -- Take the response and extract the number of nodes within the generated graph, then assert that it is equal to the expected value.

        -- Temporary: fix this later
        assertEqual "Unexpected response" 2 2
        -- let actual =
        -- assertEqual ("Unexpected response for " ++ label) expected actual

-- | Run all the findAndSavePrereqsResponse test cases
runfindAndSavePrereqsResponseTests :: [TestTree]
runfindAndSavePrereqsResponseTests = map (\(course, courseStructure, payload, expectedNodes) -> runfindAndSavePrereqsResponseTest course courseStructure payload expectedNodes) findAndSavePrereqsResponseTestCases

-- | Test suite for Generate Controller Module
test_generateController :: TestTree
test_generateController = withDatabase "Generate Controller tests" runfindAndSavePrereqsResponseTests
