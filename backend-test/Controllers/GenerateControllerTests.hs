{-|
Description: Generate Controller module tests.

Module that contains the tests for the functions in the Generate Controller module.

-}

module Controllers.GenerateControllerTests
( test_generateController
) where

import Config (runDb)
import Controllers.Generate (findAndSavePrereqsResponse)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Courses (..))
import Happstack.Server (rsBody)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, runServerPartWithGraphGenerate, withDatabase)

-- | Helper function to insert courses into the database
insertCoursesWithPrerequisites :: [(T.Text, Maybe T.Text)] -> SqlPersistM ()
insertCoursesWithPrerequisites = mapM_ insertCourse
    where
        insertCourse (code, prereqString) = insert_ (Courses code Nothing Nothing Nothing prereqString Nothing Nothing Nothing Nothing [])

-- | List of test cases as (input course, course/prereq structure, JSON payload, expected # of nodes in prereq graph)
findAndSavePrereqsResponseTestCases :: [(String, [(T.Text, Maybe T.Text)], BSL.ByteString, Integer)]
findAndSavePrereqsResponseTestCases =
    [("CSC148H1",
    [("CSC108H1", Nothing), ("CSC148H1", Just "CSC108H1")],
    "{\"courses\":[\"CSC148H1\"],\"programs\":[],\"graphOptions\":{\"taken\":[],\"departments\":[\"CSC\",\"MAT\",\"STA\"]}}",
    2
    )]

-- | Run a test case (input course, course/prereq structure, JSON payload, expected # of nodes) on the findAndSavePrereqsResponse function.
runfindAndSavePrereqsResponseTest :: String -> [(T.Text, Maybe T.Text)] -> BSL.ByteString -> Integer -> TestTree
runfindAndSavePrereqsResponseTest course graphStructure payload expected =
    testCase course $ do
        runDb $ do
            clearDatabase
            insertCoursesWithPrerequisites graphStructure
        response <- runServerPartWithGraphGenerate Controllers.Generate.findAndSavePrereqsResponse payload
        -- Take the response and extract the number of nodes (courses) within the generated graph, then assert that it is equal to the expected value.
        let body = rsBody response
            Just (Object object) = decode body
            Just (Array shapes)  = KM.lookup (K.fromString "shapes") object
            actual =
                fromIntegral . length $ filter isNode (toList shapes)

        assertEqual ("Unexpected response for " ++ course) expected actual

    where
        isNode (Object object) =
            KM.lookup (K.fromString "type_") object == Just (String "Node")
        isNode _ = False

-- | Run all the findAndSavePrereqsResponse test cases
runfindAndSavePrereqsResponseTests :: [TestTree]
runfindAndSavePrereqsResponseTests = map (\(course, courseStructure, payload, expectedNodes) -> runfindAndSavePrereqsResponseTest course courseStructure payload expectedNodes) findAndSavePrereqsResponseTestCases

-- | Test suite for Generate Controller Module
test_generateController :: TestTree
test_generateController = withDatabase "Generate Controller tests" runfindAndSavePrereqsResponseTests
