{-|
Description: Graph Controller module tests.

Module that contains the tests for the functions in the Graph Controller module.

-}

module Controllers.GraphControllerTests
( test_graphController
) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Controllers.Graph (index, saveGraphJSON, getGraphJSON)
import Data.Aeson (Value (Number, Object), decode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert_, toSqlKey)
import Database.Tables (Graph (..), Path (..), Shape (..), Text (..), SvgJSON (..))
import Happstack.Server (rsBody)
import Models.Graph (getGraph, insertGraph)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import TestHelpers (clearDatabase, mockPutRequest, runServerPart, runServerPartWith, withDatabase, mockGetRequest)
import Database.DataType (ShapeType(..))

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
insertGraphs = mapM_ insertGraph'
    where
        insertGraph' title = insert_ (Graph title 0 0 False )

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

-- | Helper function to add the default width and height to graph JSON
addDefaults :: Value -> Value
addDefaults (Object obj) = Object $
    KeyMap.insert (Key.fromString "width") (Number 256) $
    KeyMap.insert (Key.fromString "height") (Number 256) obj
addDefaults v = v

-- | List of test cases as (label, graph JSON payload)
saveGraphJSONTestCases :: [(String, BL.ByteString)]
saveGraphJSONTestCases =
    [ ("Empty graph",
        "{\"texts\":[],\"shapes\":[],\"paths\":[]}"
    ),

    ("Single shape graph",
        "{\"texts\":[],\"shapes\":[{\"graph\": 1, \"id_\": \"s1\", \"pos\": [100.0, 100.0], \"width\": 50.0, \"height\": 50.0, \"fill\": \"white\", \"stroke\": \"black\", \"text\": [], \"type_\": \"Node\", \"transform\": [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]}],\"paths\":[]}"
    ),

    ("Multi-node graph",
        "{\"texts\":[{\"graph\":1,\"rId\":\"t1\",\"pos\":[10.0,10.0],\"text\":\"Graph text 1\",\"align\":\"left\",\"fill\":\"black\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"rId\":\"t2\",\"pos\":[20.0,20.0],\"text\":\"Graph text 2\",\"align\":\"center\",\"fill\":\"blue\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}],\"shapes\":[{\"graph\":1,\"id_\":\"s1\",\"pos\":[100.0,100.0],\"width\":100.0,\"height\":50.0,\"fill\":\"white\",\"stroke\":\"black\",\"text\":[],\"type_\":\"Node\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"h2\",\"pos\":[200.0,200.0],\"width\":50.0,\"height\":10.0,\"fill\":\"green\",\"stroke\":\"blue\",\"text\":[],\"type_\":\"Hybrid\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"s3\",\"pos\":[300.0,300.0],\"width\":30.0,\"height\":30.0,\"fill\":\"red\",\"stroke\":\"purple\",\"text\":[],\"type_\":\"BoolNode\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}],\"paths\":[{\"graph\":1,\"id_\":\"p1\",\"points\":[[50.0,50.0],[150.0,50.0]],\"fill\":\"white\",\"stroke\":\"black\",\"isRegion\":false,\"source\":\"s1\",\"target\":\"h2\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]},{\"graph\":1,\"id_\":\"p2\",\"points\":[[100.0,20.0],[30.0,40.0]],\"fill\":\"yellow\",\"stroke\":\"orange\",\"isRegion\":false,\"source\":\"h2\",\"target\":\"s3\",\"transform\":[1.0,0.0,0.0,1.0,0.0,0.0]}]}"
    )
    ]

-- | Run a test case (case, graph JSON payload)
runSaveGraphJSONTest :: String -> BL.ByteString -> TestTree
runSaveGraphJSONTest label payload =
    testCase label $ do
        runDb clearDatabase
        let graphName = "Test Graph Name"
        _ <- runServerPartWith Controllers.Graph.saveGraphJSON $ mockPutRequest "/graph-save" [("nameData", T.unpack graphName), ("jsonData", BL.unpack payload)] ""
        retrievedResult <- liftIO $ Models.Graph.getGraph graphName
        let expectedValue = fmap addDefaults (decode payload :: Maybe Value)
        assertEqual ("Unexpected response for " ++ label) expectedValue retrievedResult

-- | Run all save graph test cases
runSaveGraphJSONTests :: [TestTree]
runSaveGraphJSONTests = map (uncurry runSaveGraphJSONTest) saveGraphJSONTestCases


-- | List of test cases for getGraphJSON as (label, (texts, shapes, paths))
getGraphJSONTestCases :: [(String, ([Text], [Shape], [Path]))]
getGraphJSONTestCases = 
    let testWords1 = Text {
                textGraph = toSqlKey 1,
                textRId = T.pack "t1",
                textPos = (20.0, 30.0),
                textText = T.pack "Sample Text 1",
                textAlign = T.pack "center",
                textFill = T.pack "red",
                textTransform = [1,0,0,1,0,0]
            }
        testWords2 = Text {
                textGraph = toSqlKey 1,
                textRId = T.pack "t2",
                textPos = (100.0, 60.0),
                textText = T.pack "Sample Text 2",
                textAlign = T.pack "left",
                textFill = T.pack "green",
                textTransform = [1,0,0,1,0,0]
            }
        testShape1 = Shape {
                shapeGraph = toSqlKey 1,
                shapeId_ = T.pack "s1",
                shapePos = (0.0, 0.0),
                shapeWidth = 40.0,
                shapeHeight = 60.0,
                shapeFill = T.pack "black",
                shapeStroke = T.pack "white",
                shapeText = [testWords1],
                shapeType_ = Node,
                shapeTransform = [1,0,0,1,0,0]
            }
        testShape2 = Shape {
                shapeGraph = toSqlKey 1,
                shapeId_ = T.pack "s2",
                shapePos = (100.0, 45.0),
                shapeWidth = 50.0,
                shapeHeight = 30.0,
                shapeFill = T.pack "black",
                shapeStroke = T.pack "white",
                shapeText = [testWords2],
                shapeType_ = Node,
                shapeTransform = [1,0,0,1,0,0]
            }
        testPath = Path {
                pathGraph = toSqlKey 1,
                pathId_ = T.pack "p1",
                pathPoints = [(20.0, 30.0), (125.0, 60.0)],
                pathFill = T.pack "red",
                pathStroke = T.pack "blue",
                pathIsRegion = False,
                pathSource = T.pack "s1",
                pathTarget = T.pack "s2",
                pathTransform = [1,0,0,1,0,0]
            }
    in [ 
        ("Empty graph",
        ([], [], [])
        ),

        ("Single-text-element graph",
            ([testWords1], [], [])
        ),

        ("Single-shape graph",
            ([testWords2], [testShape2], [])
        ),

        ("Two-shape connected graph",
            ([testWords1, testWords2], [testShape1, testShape2], [testPath])
        )
    ]

-- | Run a test case (case, (texts, shapes, paths)) on getGraphJSON.
runGetGraphJSONTest :: String -> ([Text], [Shape], [Path]) -> TestTree
runGetGraphJSONTest label (textlist, shapelist, pathlist) =
    testCase label $ do
        let graphName = "Test Graph Name"
        runDb $ do
            clearDatabase
            insertGraph graphName (SvgJSON textlist shapelist pathlist)
        response <- runServerPartWith Controllers.Graph.getGraphJSON $ mockGetRequest "/get-json-data" [("graphName", T.unpack graphName)] ""
        let body = rsBody response
        let jsonObj = decode body :: Maybe SvgJSON
        case jsonObj of
            Nothing -> assertFailure ("Maybe SvgJSON returned as Nothing for " ++ label)
            Just svg -> do
                assertEqual ("Texts differ for " ++ label) (show (map (\text -> text {textGraph = toSqlKey 1}) textlist)) (show (map (\text -> text {textGraph = toSqlKey 1}) (texts svg)))
                assertEqual ("Shapes differ for " ++ label) (show (map (\shape -> shape {shapeGraph = toSqlKey 1}) shapelist)) (show (map (\shape -> shape {shapeGraph = toSqlKey 1}) (shapes svg)))
                assertEqual ("Paths differ for " ++ label) (show (map (\path -> path {pathGraph = toSqlKey 1}) pathlist)) (show (map (\path -> path {pathGraph = toSqlKey 1}) (paths svg)))

-- | Run all getGraphJSON tests
runGetGraphJSONTests :: [TestTree]
runGetGraphJSONTests = map (\(label, (textlist, shapelist, pathlist)) -> runGetGraphJSONTest label (textlist, shapelist, pathlist)) getGraphJSONTestCases


-- | Test suite for Graph Controller Module
test_graphController :: TestTree
test_graphController = withDatabase "Graph Controller tests" (runIndexTests ++ runSaveGraphJSONTests ++ runGetGraphJSONTests)
