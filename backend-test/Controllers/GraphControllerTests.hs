{-|
Description: Graph Controller module tests.

Module that contains the tests for the functions in the Graph Controller module.

-}

module Controllers.GraphControllerTests
( test_graphController
) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Controllers.Graph (index, saveGraphJSON)
import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Graph (..))
import Happstack.Server (rsBody)
import Models.Graph (getGraph)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, mockPutRequest, runServerPart, runServerPartWith, withDatabase)

-- | List of test cases as (label, input graphs, expected output)
indexTestCases :: [(String, [T.Text], String)]
indexTestCases =
    [ ("No graphs",
        [],
        "[]"
    ),

    ("One graph",
        ["Statistics"],
        "[{\"dynamic\":false,\"height\":0,\"id\":1,\"title\":\"Statistics\",\"width\":0}]"
    ),

    ("Multiple graphs",
        ["Computer Science", "Statistics", "Physics"],
        "[{\"dynamic\":false,\"height\":0,\"id\":1,\"title\":\"Computer Science\",\"width\":0},{\"dynamic\":false,\"height\":0,\"id\":3,\"title\":\"Physics\",\"width\":0},{\"dynamic\":false,\"height\":0,\"id\":2,\"title\":\"Statistics\",\"width\":0}]"        )
    ]

-- | Helper function to insert graphs into the database
insertGraphs :: [T.Text] -> SqlPersistM ()
insertGraphs = mapM_ insertGraph
    where
        insertGraph title = insert_ (Graph title 0 0 False )

-- | Run a test case (case, input, expected output) on the index function.
runIndexTest :: String -> [T.Text] -> String -> TestTree
runIndexTest label graphs expected =
    testCase label $ do
        runDb $ do
            clearDatabase
            insertGraphs graphs
        response <- runServerPart Controllers.Graph.index
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the index test cases
runIndexTests :: [TestTree]
runIndexTests = map (\(label, graphs, expected) -> runIndexTest label graphs expected) indexTestCases

-- | List of test cases as (label, input JSON payload, expected JSON payload)
saveGraphJSONTestCases :: [(String, BL.ByteString, BL.ByteString)]
saveGraphJSONTestCases =
    [ ("Empty graph",
        "{\"texts\":[],\"shapes\":[],\"paths\":[]}",
        "{\"texts\":[],\"shapes\":[],\"paths\":[],\"width\":256,\"height\":256}"
    ),

    ("Single node graph",
        "{\"texts\":[],\"shapes\":[{\"graph\": 1, \"id_\": \"s1\", \"pos\": [100.0, 100.0], \"width\": 50.0, \"height\": 50.0, \"fill\": \"white\", \"stroke\": \"black\", \"text\": [], \"type_\": \"Node\", \"transform\": [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]}],\"paths\":[]}",
        "{\"texts\":[],\"shapes\":[{\"graph\": 1, \"id_\": \"s1\", \"pos\": [100.0, 100.0], \"width\": 50.0, \"height\": 50.0, \"fill\": \"white\", \"stroke\": \"black\", \"text\": [], \"type_\": \"Node\", \"transform\": [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]}],\"paths\":[],\"width\":256,\"height\":256}"
    ),

    ("Multi-node graph",
        "{\"texts\":[{\"graph\":1,\"rId\":\"t1\",\"pos\":[10.0,10.0],\"text\":\"Graph text 1\",\"align\":\"left\",\"fill\":\"black\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"rId\":\"t2\",\"pos\":[100.0,100.0],\"text\":\"Graph text 2\",\"align\":\"center\",\"fill\":\"blue\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}],\"shapes\":[{\"graph\":1,\"id_\":\"s1\",\"pos\":[100.0,100.0],\"width\":100.0,\"height\":50.0,\"fill\":\"white\",\"stroke\":\"black\",\"text\":[],\"type_\":\"Node\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"h2\",\"pos\":[200.0,200.0],\"width\":50.0,\"height\":10.0,\"fill\":\"green\",\"stroke\":\"blue\",\"text\":[],\"type_\":\"Hybrid\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"s3\",\"pos\":[300.0,300.0],\"width\":30.0,\"height\":30.0,\"fill\":\"red\",\"stroke\":\"purple\",\"text\":[],\"type_\":\"BoolNode\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}],\"paths\":[{\"graph\":1,\"id_\":\"p1\",\"points\":[[50.0,50.0],[150.0,50.0]],\"fill\":\"white\",\"stroke\":\"black\",\"isRegion\":false,\"source\":\"s1\",\"target\":\"h2\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"p2\",\"points\":[[100.0,20.0],[30.0,40.0]],\"fill\":\"yellow\",\"stroke\":\"orange\",\"isRegion\":true,\"source\":\"h2\",\"target\":\"s3\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}]}",
        "{\"texts\":[{\"graph\":1,\"rId\":\"t1\",\"pos\":[10.0,10.0],\"text\":\"Graph text 1\",\"align\":\"left\",\"fill\":\"black\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"rId\":\"t2\",\"pos\":[100.0,100.0],\"text\":\"Graph text 2\",\"align\":\"center\",\"fill\":\"blue\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"rId\":\"t1\",\"pos\":[10.0,10.0],\"text\":\"Graph text 1\",\"align\":\"left\",\"fill\":\"black\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}],\"shapes\":[{\"graph\":1,\"id_\":\"s1\",\"pos\":[100.0,100.0],\"width\":100.0,\"height\":50.0,\"fill\":\"white\",\"stroke\":\"black\",\"text\":[{\"graph\":1,\"rId\":\"t2\",\"pos\":[100.0,100.0],\"text\":\"Graph text 2\",\"align\":\"center\",\"fill\":\"blue\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}],\"type_\":\"Node\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"h2\",\"pos\":[200.0,200.0],\"width\":50.0,\"height\":10.0,\"fill\":\"green\",\"stroke\":\"blue\",\"text\":[],\"type_\":\"Hybrid\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"s3\",\"pos\":[300.0,300.0],\"width\":30.0,\"height\":30.0,\"fill\":\"red\",\"stroke\":\"purple\",\"text\":[],\"type_\":\"BoolNode\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}],\"paths\":[{\"graph\":1,\"id_\":\"p1\",\"points\":[[50.0,50.0],[150.0,50.0]],\"fill\":\"white\",\"stroke\":\"black\",\"isRegion\":false,\"source\":\"s1\",\"target\":\"h2\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"p2p2\",\"points\":[[100.0,20.0],[30.0,40.0]],\"fill\":\"yellow\",\"stroke\":\"orange\",\"isRegion\":true,\"source\":\"\",\"target\":\"\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"p2p2\",\"points\":[[100.0,20.0],[30.0,40.0]],\"fill\":\"yellow\",\"stroke\":\"orange\",\"isRegion\":true,\"source\":\"\",\"target\":\"\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}],\"width\":256,\"height\":256}"
    )
    ]

-- | Run a test case (case, input JSON payload, expected JSON payload)
runSaveGraphJSONTest :: String -> BL.ByteString -> BL.ByteString -> TestTree
runSaveGraphJSONTest label inputPayload expectedPayload =
    testCase label $ do
        runDb clearDatabase
        let graphName = "Test Graph Name"
        _ <- runServerPartWith Controllers.Graph.saveGraphJSON $ mockPutRequest "/graph-save" [("nameData", T.unpack graphName), ("jsonData", BL.unpack inputPayload)] ""
        retrievedResult <- liftIO $ Models.Graph.getGraph graphName
        let expectedValue = decode expectedPayload :: Maybe Value
        assertEqual ("Unexpected response for " ++ label) expectedValue retrievedResult

-- | Run all save graph test cases
runSaveGraphJSONTests :: [TestTree]
runSaveGraphJSONTests = map(\(label, inputPayload, expectedPayload) -> runSaveGraphJSONTest label inputPayload expectedPayload) saveGraphJSONTestCases

-- | Test suite for Graph Controller Module
test_graphController :: TestTree
test_graphController = withDatabase "Graph Controller tests" (runIndexTests ++ runSaveGraphJSONTests)
