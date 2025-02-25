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

-- A list of texts for rects
rectTextMocks :: [Text]
rectTextMocks = [
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (50.0, 100.0), textText = T.pack "CSC108", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = "", textPos = (200.9999, 89.99997), textText = "CSC148/", textAlign = "", textFill = "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = "", textPos = (201.92939, 90.8812), textText = "CSC111", textAlign = "", textFill = "", textTransform = [1,0,0,1,0,0]}
    ]

-- A list of texts for rects to test rotation/skewing transformations
rectShearTextMocks :: [Text]
rectShearTextMocks = [
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (1000.0, 1000.0), textText = T.pack "CSC110", textAlign = T.pack "", textFill = T.pack "", textTransform = [0,1,-1,0,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (1000.0, 1000.0), textText = T.pack "CSC110", textAlign = T.pack "", textFill = T.pack "", textTransform = [0,-1,1,0,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (1000.0, 1000.0), textText = T.pack "CSC110", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0.5,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (1000.0, 1000.0), textText = T.pack "CSC110", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,-1.2,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (1000.0, 1000.0), textText = T.pack "CSC110", textAlign = T.pack "", textFill = T.pack "", textTransform = [1.1,0.5,1.2,1.1,0,0]}
    ]

-- A list of texts for ellipses
boolTextMocks :: [Text]
boolTextMocks = [
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (0.0, 0.0), textText = T.pack "and", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (98.0, 90.0), textText = T.pack "and", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (210.0, 205.99), textText = T.pack "and", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (300.0, 300.0), textText = T.pack "and", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (301.0, 301.0), textText = T.pack "or", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]},
        Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (410.0, 410.0), textText = T.pack "or", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]}
    ]

-- A list of rects (type Node and Hybrid)
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
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (0.0, 0.0), shapeWidth = 10, shapeHeight = 10, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (990.0, 990.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]}
    ]

-- A list of ellipses
ellipseMocks :: [Shape]
ellipseMocks = [
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (0.0, 0.0), shapeWidth = 25, shapeHeight = 20, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = BoolNode, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (100.0, 100.0), shapeWidth = 24, shapeHeight = 20, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = BoolNode, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (200.5, 200.5), shapeWidth = 25, shapeHeight = 20, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = BoolNode, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (300.99, 300.51), shapeWidth = 25, shapeHeight = 20, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = BoolNode, shapeTransform = [1,0,0,1,0,0]},
        Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (400.99, 400.51), shapeWidth = 25, shapeHeight = 20, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = BoolNode, shapeTransform = [1,0,0,1,0,0]}
    ]


-- * Test Cases
-- * The first element in the tuple is the inputs, the second is the expected ouput.

-- Test cases for buildRect with no transformations.
buildRectNoTransformInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectNoTransformInputs = [
        ((rectTextMocks, 1, head rectMocks), (T.pack "csc108", [head rectTextMocks])), -- one Text intersecting at corner
        ((rectTextMocks, 2, rectMocks !! 1), (T.pack "h2", [head rectTextMocks])), -- one Text intersecting at border x
        ((rectTextMocks, 3, rectMocks !! 2), (T.pack "csc108", [head rectTextMocks])), -- one Text intersecting at border
        ((rectTextMocks, 4, rectMocks !! 3), (T.pack "csc108", [head rectTextMocks])), -- one Text intersecting within shape area
        ((rectTextMocks, 5, rectMocks !! 4), (T.pack "", [])), -- no intersection for node
        ((rectTextMocks, 6, rectMocks !! 5), (T.pack "h6", [])), -- no intersection for hybrid
        ((rectTextMocks, 7, rectMocks !! 6), (T.pack "csc148csc111", [rectTextMocks !! 1, rectTextMocks !! 2])), -- multiple text intersections for node
        ((rectTextMocks, 8, rectMocks !! 7), (T.pack "h8", [head rectTextMocks, rectTextMocks !! 1, rectTextMocks !! 2])) -- multiple text intersections for hybrid
    ]

-- Test cases for buildRect with translation.
buildRectTranslationInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectTranslationInputs = [
        ((rectTextMocks, 1, setTransformation (rectMocks !! 1) [1,0,0,1,50,0]), (T.pack "h1", [head rectTextMocks])), -- translate x
        ((rectTextMocks, 2, setTransformation (head rectMocks) [1,0,0,1,0,-30]), (T.pack "csc108", [head rectTextMocks])), -- translate y
        ((rectTextMocks, 3, setTransformation (rectMocks !! 2) [1,0,0,1,-40,15]), (T.pack "csc108", [head rectTextMocks])), -- translate xy
        ((rectTextMocks, 4, setTransformation (head rectMocks) [1,0,0,1,1,1]), (T.pack "", [])), -- no intersection
        ((rectTextMocks, 5, setTransformation (rectMocks !! 7) [1,0,0,1,200,89]), (T.pack "h5", [rectTextMocks !! 1, rectTextMocks !! 2])) -- multiple texts
    ]

-- Test cases for buildRect with scaling.
buildRectScaleInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectScaleInputs = [
        ((rectTextMocks, 1, setTransformation (rectMocks !! 9) [50,0,0,1,0,0]), (T.pack "csc108", [head rectTextMocks])), -- scale x
        ((rectTextMocks, 2, setTransformation (rectMocks !! 8) [1,0,0,100,0,0]), (T.pack "csc108", [head rectTextMocks])), -- scale y
        ((rectTextMocks, 3, setTransformation (rectMocks !! 10) [49,0,0,99,0,0]), (T.pack "csc108", [head rectTextMocks])), -- scale xy
        ((rectTextMocks, 4, setTransformation (head rectMocks) [-1,0,0,1,0,0]), (T.pack "", [])), -- reflect x, no intersection
        ((rectTextMocks, 5, setTransformation (head rectMocks) [0,0,0,-1,0,0]), (T.pack "", [])), -- reflect y, no intersection
        ((rectTextMocks, 6, setTransformation (rectMocks !! 4) [-0.1,0,0,0.9,0,0]), (T.pack "", [])), -- reflect xy
        ((rectTextMocks, 7, setTransformation (head rectMocks) [0.1,0,0,1.5,0,0]), (T.pack "", [])), -- no intersection
        ((rectTextMocks, 8, setTransformation (rectMocks !! 10) [199,0,0,88,0,0]), (T.pack "csc148csc111", [rectTextMocks !! 1, rectTextMocks !! 2])), -- multiple texts
        ((rectTextMocks, 9, setTransformation (rectMocks !! 11) [100,0,0,100,0,0]), (T.pack "csc108csc148csc111", [head rectTextMocks, rectTextMocks !! 1, rectTextMocks !! 2])) -- on (0,0)
    ]

-- Test cases for buildRect with rotation/skewing.
buildRectShearInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectShearInputs = [
        (([head rectShearTextMocks], 1, setTransformation (rectMocks !! 12) [0,1,-1,0,0,0]), (T.pack "csc110", [head rectShearTextMocks])), -- CW rotation
        (([rectShearTextMocks !! 1], 2, setTransformation (rectMocks !! 12) [0,-1,1,0,0,0]), (T.pack "csc110", [rectShearTextMocks !! 1])), -- CWW rotation
        (([rectShearTextMocks !! 2], 3, setTransformation (rectMocks !! 12) [1,0.5,0,1,0,0]), (T.pack "csc110", [rectShearTextMocks !! 2])), -- skew x
        (([rectShearTextMocks !! 3], 4, setTransformation (rectMocks !! 12) [1,0,-1.2,1,0,0]), (T.pack "csc110", [rectShearTextMocks !! 3])), -- skew y
        (([rectShearTextMocks !! 4], 5, setTransformation (rectMocks !! 12) [1.1,0.5,1.2,1.1,0,0]), (T.pack "csc110", [rectShearTextMocks !! 4])) -- skew xy
    ]

-- Test cases for buildRect with a mixture of different transformations.
buildRectMixedInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildRectMixedInputs = [
        -- ((textMocks, 1, setTransformation (head rectMocks) [1.5,0,0,1.5,-30,-60]), (T.pack "csc108", [head textMocks])),
        -- ((textMocks, 2, setTransformation (rectMocks !! 2) [-1,0,0,1.2,50,-20]), (T.pack "csc108", [head textMocks])),
        -- ((textMocks, 3, setTransformation (head rectMocks) [1,0,0,-0.8,100,158]), (T.pack "csc148csc111", [textMocks !! 1, textMocks !! 2])),
        -- ((textMocks, 4, setTransformation (rectMocks !! 4) [2.5,0,0,3.5,5,5]), (T.pack "", []))
    ]

-- Test cases for buildEllipses with no transformations.
buildEllipsesNoTransformationInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildEllipsesNoTransformationInputs = [
        ((boolTextMocks, 1, head ellipseMocks), (T.pack "bool1", [head boolTextMocks])), -- within the region, i.e. calulation in intersectsEllipse < 1
        ((boolTextMocks, 2, ellipseMocks !! 1), (T.pack "bool2", [])), -- on the border, i.e. calculation in intersectsEllipse = 1
        ((boolTextMocks, 3, ellipseMocks !! 2), (T.pack "bool3", [])), -- outside the region, i.e. calculation in intersectsEllipse > 1
        ((boolTextMocks, 4, ellipseMocks !! 3), (T.pack "bool4", [boolTextMocks !! 3, boolTextMocks !! 4])) -- multiple texts within the region
    ]

-- Test cases for buildEllipses with translation.
buildEllipsesTranslationInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildEllipsesTranslationInputs = [
        ((boolTextMocks, 1, setTransformation (head ellipseMocks) [1,0,0,1,10,0]), (T.pack "bool1", [head boolTextMocks])), -- translate x
        ((boolTextMocks, 2, setTransformation (head ellipseMocks) [1,0,0,1,0,-10]), (T.pack "bool2", [])), -- translate y, no intersection
        ((boolTextMocks, 3, setTransformation (head ellipseMocks) [1,0,0,1,300,300]), (T.pack "bool3", [boolTextMocks !! 3, boolTextMocks !! 4])) -- translate xy, multiple texts
    ]


-- Test cases for buildEllipses with scaling (scale origin at center of ellipse).
buildEllipsesScaleInputs :: [(([Text], Integer, Shape), (T.Text, [Text]))]
buildEllipsesScaleInputs = [
        ((boolTextMocks, 1, setTransformation (ellipseMocks !! 2) [1.1,0,0,1,0,0]), (T.pack "bool1", [boolTextMocks !! 2])), -- scale x
        ((boolTextMocks, 2, setTransformation (ellipseMocks !! 2) [1,0,0,1.02,0,0]), (T.pack "bool2", [boolTextMocks !! 2])), -- scale y
        ((boolTextMocks, 3, setTransformation (ellipseMocks !! 3) [1.39,0,0,1.39,0,0]), (T.pack "bool3", [boolTextMocks !! 5])), -- scale xy
        ((boolTextMocks, 4, setTransformation (ellipseMocks !! 3) [-1,0,0,1,0,0]), (T.pack "bool4", [])), -- reflect x
        ((boolTextMocks, 5, setTransformation (ellipseMocks !! 3) [1,0,0,-0.01,0,0]), (T.pack "bool5", [])), -- reflect y
        ((boolTextMocks, 6, setTransformation (ellipseMocks !! 3) [-0.2,0,0,-200,0,0]), (T.pack "bool6", [])), -- reflect xy
        ((boolTextMocks, 7, setTransformation (head ellipseMocks) [1000,0,0,1000,0,0]), (T.pack "bool7", boolTextMocks)) -- big scale
    ]


-- TODO: add tests for text transformations on either buildRect or buildEllipse


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

-- Function for testing a shape builder's (buildRect and buildEllipses) test case
testShapeBuilder :: ([Text] -> Shape -> Integer -> Shape)
                 -> String
                 -> String
                 -> (([Text], Integer, Shape), (T.Text, [Text]))
                 -> Test
testShapeBuilder fn label shapeLabel input =
    TestLabel label $ TestCase $ do
        let ((texts, elementId, rect), (expectedId_, expectedTexts)) = input
            result = fn texts rect elementId
        assertEqual ("Check id_ for " ++ shapeLabel ++ " " ++ show elementId) expectedId_ $ shapeId_ result
        assertBool ("Check texts for rect " ++ shapeLabel ++ " " ++ show elementId) $ compareTexts expectedTexts $ shapeText result

-- Run all test cases for buildRect
runBuildRectTests :: [Test]
runBuildRectTests =
    map (testShapeBuilder buildRect "Test buildRect no transformation" "rect") buildRectNoTransformInputs ++
    map (testShapeBuilder buildRect "Test buildRect translation" "rect") buildRectTranslationInputs ++
    map (testShapeBuilder buildRect "Test buildRect scaling" "rect") buildRectScaleInputs ++
    map (testShapeBuilder buildRect "Test buildRect rotation/skewing" "rect") buildRectShearInputs ++
    map (testShapeBuilder buildRect "Test buildRect mixed transformations" "rect") buildRectMixedInputs

-- Run all test cases for buildEllipses
runBuildEllipsesTests :: [Test]
runBuildEllipsesTests =
    map (testShapeBuilder buildEllipses "Test buildEllipses no transformation" "ellipse") buildEllipsesNoTransformationInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses translation" "ellipse") buildEllipsesTranslationInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses scale" "ellipse") buildEllipsesScaleInputs


-- Test suite for intersection checks
intersectionTestSuite :: Test
intersectionTestSuite = TestLabel "Intersection tests" $
    TestList $ runBuildRectTests ++ runBuildEllipsesTests