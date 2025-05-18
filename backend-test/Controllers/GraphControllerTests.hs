{-|
Description: Course Controller module tests.

Module that contains the tests for the functions in the Course Controller module.

-}

module Controllers.GraphControllerTests
( graphControllerTestSuite
) where

import Config (runDb)
import Controllers.Graph (index)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Graph (..))
import Happstack.Server (rsBody)
import Test.HUnit (Test (..), assertEqual)
import TestHelpers (clearDatabase, runServerPart)

-- | List of test cases as (label, input graphs, expected output)
indexTestCases :: [(String, [T.Text], String)]
indexTestCases =
    [ ("No graphs",
        [],
        "[]"
    ),

    ("One department",
        ["Statistics"],
        "[{\"dynamic\":false,\"height\":0,\"id\":1,\"title\":\"Statistics\",\"width\":0}]"
    ),

    ("Multiple departments",
        ["Computer Science", "Statistics", "Physics"],
        "[{\"dynamic\":false,\"height\":0,\"id\":1,\"title\":\"Computer Science\",\"width\":0},{\"dynamic\":false,\"height\":0,\"id\":3,\"title\":\"Physics\",\"width\":0},{\"dynamic\":false,\"height\":0,\"id\":2,\"title\":\"Statistics\",\"width\":0}]"        )
    ]

-- | Helper function to insert graphs into the database
insertGraphs :: [T.Text] -> SqlPersistM ()
insertGraphs = mapM_ insertGraph
    where
        insertGraph title = insert_ (Graph title 0 0 False )

-- | Run a test case (case, input, expected output) on the index function.
runIndexTest :: String -> [T.Text] -> String -> Test
runIndexTest label graphs expected =
    TestLabel label $ TestCase $ do
        runDb $ do
            clearDatabase
            insertGraphs graphs
        response <- runServerPart Controllers.Graph.index
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the index test cases
runIndexTests :: [Test]
runIndexTests = map (\(label, graphs, expected) -> runIndexTest label graphs expected) indexTestCases

-- | Test suite for Course Controller Module
graphControllerTestSuite :: Test
graphControllerTestSuite = TestLabel "Course Controller tests" $ TestList runIndexTests
