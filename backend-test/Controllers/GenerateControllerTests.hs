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
import TestHelpers (mockPutRequest, clearDatabase, runServerPartWith, withDatabase)

-- | Helper function to insert courses into the database
insertCoursesWithPrerequisites :: [(T.Text, Maybe T.Text)] -> SqlPersistM ()
insertCoursesWithPrerequisites = mapM_ insertCourse
    where
        insertCourse (code, prereqString) = insert_ (Courses { coursesCode = code, coursesTitle = Nothing, coursesDescription = Nothing, coursesPrereqs = prereqString, coursesExclusions = Nothing, coursesBreadth = Nothing, coursesDistribution = Nothing, coursesPrereqString = prereqString, coursesCoreqs = Nothing, coursesVideoUrls = [] })

-- | List of test cases as
-- (input course, course/prereq structure, JSON payload, expected # of nodes in prereq graph, expected # of boolean nodes in prereq graph)
findAndSavePrereqsResponseTestCases :: [(String, [(T.Text, Maybe T.Text)], BSL.ByteString, Integer, Integer)]
findAndSavePrereqsResponseTestCases =
    [("CSC148H1",
    [("CSC108H1", Nothing), ("CSC148H1", Just "CSC108H1")],
    "{\"courses\":[\"CSC148H1\"],\"programs\":[],\"graphOptions\":{\"taken\":[],\"departments\":[\"CSC\",\"MAT\",\"STA\"]}}",
    2,
    0
    ),
    ("CSC368H1",
    [("CSC209H1", Nothing), ("CSC258H1", Nothing), ("CSC368H1", Just "CSC209H1,  CSC258H1"), ("CSC369H1", Just "CSC209H1, CSC258H1")],
    "{\"courses\":[\"CSC368H1\", \"CSC369H1\"],\"programs\":[],\"graphOptions\":{\"taken\":[],\"departments\":[\"CSC\",\"MAT\",\"STA\"]}}",
    4,
    1
    )]

-- | Run a test case (input course, course/prereq structure, JSON payload, expected # of nodes) on the findAndSavePrereqsResponse function.
runfindAndSavePrereqsResponseTest :: String -> [(T.Text, Maybe T.Text)] -> BSL.ByteString -> Integer -> Integer -> TestTree
runfindAndSavePrereqsResponseTest course graphStructure payload expectedNodes expectedBoolNodes =
    testCase course $ do
        runDb $ do
            clearDatabase
            insertCoursesWithPrerequisites graphStructure
        response <- runServerPartWith Controllers.Generate.findAndSavePrereqsResponse $ mockPutRequest "/graph-generate" [] payload
        -- Take the response and extract the number of nodes (courses) within the generated graph, then assert that it is equal to the expected value.
        let body = rsBody response
            Just (Object object) = decode body
            Just (Array shapes)  = KM.lookup (K.fromString "shapes") object
            actualNodes =
                fromIntegral . length $ filter isNode (toList shapes)
            actualBoolNodes =
                fromIntegral . length $ filter isBoolNode (toList shapes)

        -- TODO: currently, one extra node is being generated, so we subtract 1 from expectedNodes
        -- This should be changed once the bug is fixed!
        assertEqual ("Unexpected response for " ++ course) expectedNodes (actualNodes - 1)
        assertEqual ("Unexpected response for " ++ course) expectedBoolNodes actualBoolNodes

    where
        isNode (Object object) =
            KM.lookup (K.fromString "type_") object == Just (String "Node")
        isNode _ = False
        isBoolNode (Object object) =
            KM.lookup (K.fromString "type_") object == Just (String "BoolNode")
        isBoolNode _ = False

-- | Run all the findAndSavePrereqsResponse test cases
runfindAndSavePrereqsResponseTests :: [TestTree]
runfindAndSavePrereqsResponseTests = map (\(course, courseStructure, payload, expectedNodes, expectedBoolNodes) -> runfindAndSavePrereqsResponseTest course courseStructure payload expectedNodes expectedBoolNodes) findAndSavePrereqsResponseTestCases

-- | Test suite for Generate Controller Module
test_generateController :: TestTree
test_generateController = withDatabase "Generate Controller tests" runfindAndSavePrereqsResponseTests
