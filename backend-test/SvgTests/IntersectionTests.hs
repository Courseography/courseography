{-|
Description: Test entity intersections.

Module that contains the tests for intersection checks in the SVG Builder module.

-}

module SvgTests.IntersectionTests (intersectionTestSuite) where

import Svg.Builder (buildRect, buildEllipses, buildPath, intersectsWithShape)
import Database.Tables (Text(..), Shape(..))
import Database.DataType (ShapeType(..))
import qualified Data.Text as T
import Test.HUnit (Test(..), assertEqual, assertBool)
import Database.Persist.Sqlite (toSqlKey)

-- * Mocks

-- A list of mocked tests
textMocks :: [Text]
textMocks = [
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (50.0, 100.0), textText = T.pack "CSC108", textAlign = T.pack "", textFill = T.pack ""},
        Text { textGraph = toSqlKey 1, textRId = "", textPos = (200.9999, 89.99997), textText = "CSC148/", textAlign = "", textFill = ""},
        Text { textGraph = toSqlKey 1, textRId = "", textPos = (201.92939, 90.8812), textText = "CSC111", textAlign = "", textFill = ""}
    ]


-- * Test Cases

-- Test cases for buildRect. The first element in the tuple is the inputs, the second is the expected ouput.
buildRectInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectInputs = [
        ((textMocks, 1, Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (50.0, 100.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node}),
          (T.pack "csc108", [head textMocks])), -- one Text intersecting at corner
        ((textMocks, 2, Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack  "", shapePos = (0.0, 100.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Hybrid}),
          (T.pack "h2", [head textMocks])), -- one Text intersecting at border x
        ((textMocks, 3, Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack  "", shapePos = (50.0, 80.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node}),
          (T.pack "csc108", [head textMocks])), -- one Text intersecting at border y
        ((textMocks, 4, Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (45.9998, 90.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node}),
          (T.pack "csc108", [head textMocks])), -- one Text intersecting within shape area
        ((textMocks, 5, Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (51.0, 101.56), shapeWidth = 20, shapeHeight = 10, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node}),
          (T.pack "", [])), -- no intersection for node
        ((textMocks, 6, Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (51.0, 101.56), shapeWidth = 20, shapeHeight = 10, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Hybrid}),
          (T.pack "h6", [])), -- no intersection for hybrid
        ((textMocks, 7, Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (199.8863, 88.1213), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node}),
          (T.pack "csc148csc111", [textMocks !! 1, textMocks !! 2])) ,-- multiple text intersections for node
        ((textMocks, 8, Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (0.0, 0.0), shapeWidth = 202, shapeHeight = 202, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Hybrid}),
          (T.pack "h8", [head textMocks, textMocks !! 1, textMocks !! 2])) -- multiple text intersections for hybrid
    ]


-- Helper to compare if two Text types
compareTexts :: [Text] -> [Text] -> Bool
compareTexts expected actual
    | length expected /= length actual = False
    | otherwise = all compareTextPair (zip expected actual)
    where
        compareTextPair (expectedText, actualText) =
            textGraph expectedText == textGraph actualText &&
            textRId expectedText == textRId actualText &&
            textPos expectedText == textPos actualText &&
            textText expectedText == textText actualText &&
            textAlign expectedText == textAlign actualText &&
            textFill expectedText == textFill actualText


-- * Test Runners

-- function for testing a build rect test case
testBuildRect :: String -> (([Text], Integer, Shape), (T.Text, [Text])) -> Test
testBuildRect label input =
    TestLabel label $ TestCase $ do
        let ((texts, elementId, rect), (expectedId_, expectedTexts)) = input
            result = buildRect texts rect elementId
        assertEqual ("Check id_ for rect " ++ show elementId) expectedId_ $ shapeId_ result
        assertBool ("Check texts for rect " ++ show elementId) $ compareTexts expectedTexts $ shapeText result

runBuildRectTests :: [Test]
runBuildRectTests = map (testBuildRect "Test buildRect") buildRectInputs

intersectionTestSuite :: Test
intersectionTestSuite = TestLabel "Intersection tests" $ TestList runBuildRectTests