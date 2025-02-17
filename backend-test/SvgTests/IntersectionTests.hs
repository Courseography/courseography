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
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (50.0, 100.0), textText = T.pack "CSC108", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = "", textPos = (200.9999, 89.99997), textText = "CSC148/", textAlign = "", textFill = "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = "", textPos = (201.92939, 90.8812), textText = "CSC111", textAlign = "", textFill = "", textTransform = [1,0,0,1,0,0]}
    ]

-- A list of mocked rects (type Node and Hybrid)
rectMocks :: [Shape]
rectMocks = [
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (50.0, 100.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (0.0, 100.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Hybrid, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (50.0, 80.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (45.9998, 90.0), shapeWidth = 30, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (80.0, 101.56), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (80.0, 101.56), shapeWidth = 20, shapeHeight = 10, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Hybrid, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (199.8863, 88.1213), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (0.0, 0.0), shapeWidth = 202, shapeHeight = 202, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Hybrid, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (50.0, 1.0), shapeWidth = 10, shapeHeight = 10, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (1.0, 100.0), shapeWidth = 10, shapeHeight = 10, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (1.0, 1.0), shapeWidth = 10, shapeHeight = 10, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (0.0, 0.0), shapeWidth = 10, shapeHeight = 10, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]}
    ]


-- * Test Cases
-- * The first element in the tuple is the inputs, the second is the expected ouput.

-- Test cases for buildRect with no transformations.
buildRectNoTransformInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectNoTransformInputs = [
        ((textMocks, 1, head rectMocks), (T.pack "csc108", [head textMocks])), -- one Text intersecting at corner
        ((textMocks, 2, rectMocks !! 1), (T.pack "h2", [head textMocks])), -- one Text intersecting at border x
        ((textMocks, 3, rectMocks !! 2), (T.pack "csc108", [head textMocks])), -- one Text intersecting at border
        ((textMocks, 4, rectMocks !! 3), (T.pack "csc108", [head textMocks])), -- one Text intersecting within shape area
        ((textMocks, 5, rectMocks !! 4), (T.pack "", [])), -- no intersection for node
        ((textMocks, 6, rectMocks !! 5), (T.pack "h6", [])), -- no intersection for hybrid
        ((textMocks, 7, rectMocks !! 6), (T.pack "csc148csc111", [textMocks !! 1, textMocks !! 2])), -- multiple text intersections for node
        ((textMocks, 8, rectMocks !! 7), (T.pack "h8", [head textMocks, textMocks !! 1, textMocks !! 2])) -- multiple text intersections for hybrid
    ]

-- Test cases for buildRect with translation.
buildRectTranslationInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectTranslationInputs = [
        ((textMocks, 1, setTransformation (rectMocks !! 1) [1,0,0,1,50,0]), (T.pack "h1", [head textMocks])), -- translate x
        ((textMocks, 2, setTransformation (head rectMocks) [1,0,0,1,0,-30]), (T.pack "csc108", [head textMocks])), -- translate y
        ((textMocks, 3, setTransformation (rectMocks !! 2) [1,0,0,1,-40,15]), (T.pack "csc108", [head textMocks])), -- translate xy
        ((textMocks, 4, setTransformation (head rectMocks) [1,0,0,1,1,1]), (T.pack "", [])), -- no intersection
        ((textMocks, 5, setTransformation (rectMocks !! 7) [1,0,0,1,200,89]), (T.pack "h5", [textMocks !! 1, textMocks !! 2])) -- multiple texts
    ]

-- Test cases for buildRect with scaling.
buildRectScaleInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectScaleInputs = [
        ((textMocks, 1, setTransformation (rectMocks !! 9) [50,0,0,1,0,0]), (T.pack "csc108", [head textMocks])), -- scale x
        ((textMocks, 2, setTransformation (rectMocks !! 8) [1,0,0,100,0,0]), (T.pack "csc108", [head textMocks])), -- scale y
        ((textMocks, 3, setTransformation (rectMocks !! 10) [49,0,0,99,0,0]), (T.pack "csc108", [head textMocks])), -- scale xy
        ((textMocks, 4, setTransformation (head rectMocks) [-1,0,0,1,0,0]), (T.pack "", [])), -- reflect x, no intersection
        ((textMocks, 5, setTransformation (head rectMocks) [0,0,0,-1,0,0]), (T.pack "", [])), -- reflect y, no intersection
        ((textMocks, 6, setTransformation (rectMocks !! 4) [-0.1,0,0,0.9,0,0]), (T.pack "csc108", [head textMocks])), -- reflect xy
        ((textMocks, 7, setTransformation (head rectMocks) [0.1,0,0,1.5,0,0]), (T.pack "", [])), -- no intersection
        ((textMocks, 8, setTransformation (rectMocks !! 10) [199,0,0,88,0,0]), (T.pack "csc148csc111", [textMocks !! 1, textMocks !! 2])), -- multiple texts
        ((textMocks, 9, setTransformation (rectMocks !! 11) [100,0,0,100,0,0]), (T.pack "", [])) -- on (0,0)
    ]

-- Test cases for buildRect with a mixture of different transformations.
buildRectMixedInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectMixedInputs = [
        ((textMocks, 1, setTransformation (head rectMocks) [1.5,0,0,1.5,-30,-60]), (T.pack "csc108", [head textMocks])),
        ((textMocks, 2, setTransformation (rectMocks !! 2) [-1,0,0,1.2,50,-20]), (T.pack "csc108", [head textMocks])),
        ((textMocks, 3, setTransformation (head rectMocks) [1,0,0,-0.8,100,158]), (T.pack "csc148csc111", [textMocks !! 1, textMocks !! 2])),
        ((textMocks, 4, setTransformation (rectMocks !! 4) [2.5,0,0,3.5,5,5]), (T.pack "", []))
    ]


-- * Helpers

-- Helper to modify tranformation for a shape
setTransformation :: Shape -> [Double] -> Shape
setTransformation entity transformation = entity {shapeTransform = transformation}

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
            textFill expectedText == textFill actualText &&
            textTransform expectedText == textTransform actualText


-- * Test Runners

-- Function for testing a build rect test case
testBuildRect :: String -> (([Text], Integer, Shape), (T.Text, [Text])) -> Test
testBuildRect label input =
    TestLabel label $ TestCase $ do
        let ((texts, elementId, rect), (expectedId_, expectedTexts)) = input
            result = buildRect texts rect elementId
        assertEqual ("Check id_ for rect " ++ show elementId) expectedId_ $ shapeId_ result
        assertBool ("Check texts for rect " ++ show elementId) $ compareTexts expectedTexts $ shapeText result

-- Run all test cases for buildRect
runBuildRectTests :: [Test]
runBuildRectTests =
    map (testBuildRect "Test buildRect no transformation") buildRectNoTransformInputs ++
    map (testBuildRect "Test buildRect translation") buildRectTranslationInputs ++
    map (testBuildRect "Test buildRect scaling") buildRectScaleInputs ++
    map (testBuildRect "Test buildRect scaling") buildRectMixedInputs

-- Test suite for intersection checks
intersectionTestSuite :: Test
intersectionTestSuite = TestLabel "Intersection tests" $ TestList runBuildRectTests