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

-- Text mocks
defaultRectText :: Text
defaultRectText = Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (50.0, 100.0), textText = T.pack "CSC108", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]}

defaultRectText2 :: Text
defaultRectText2 = Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (201.92939, 90.8812), textText = T.pack "CSC148", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]}

defaultEllipseText :: Text
defaultEllipseText = Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (0.0, 0.0), textText = T.pack "and", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]}

defaultEllipseText2 :: Text
defaultEllipseText2 = Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (301.0, 301.0), textText = T.pack "or", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0]}


-- Shape mocks
defaultRect :: Shape
defaultRect = Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (50.0, 100.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0]}

defaultEllipse :: Shape
defaultEllipse = Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (0.0, 0.0), shapeWidth = 25, shapeHeight = 20, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = BoolNode, shapeTransform = [1,0,0,1,0,0]}


-- * Test Cases
-- * The first element in the tuple is the inputs, the second is the expected ouput.

-- Test cases for buildRect with no transformations.
buildRectNoTransformInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildRectNoTransformInputs = [
        ((1, [defaultRectText, defaultRectText2], defaultRect),
         (T.pack "csc108", [defaultRectText]), "one Text intersecting at corner"),
        ((2, [defaultRectText, defaultRectText2], defaultRect { shapePos = (0.0, 100.0), shapeType_ = Hybrid }),
         (T.pack "h2", [defaultRectText]), "one Text intersecting at border x"),
        ((3, [defaultRectText, defaultRectText2], defaultRect { shapePos = (50.0, 80.0) }),
         (T.pack "csc108", [defaultRectText]), "one Text intersecting at border"),
        ((4, [defaultRectText, defaultRectText2], defaultRect { shapePos = (45.9998, 90.0), shapeWidth = 30, shapeHeight = 30 }),
         (T.pack "csc108", [defaultRectText]), "one Text intersecting within shape area"),
        ((5, [defaultRectText, defaultRectText2], defaultRect { shapePos = (80.0, 101.56) }),
         (T.pack "", []), "no intersection for node"),
        ((6, [defaultRectText, defaultRectText2], defaultRect { shapePos = (80.0, 101.56), shapeType_ = Hybrid, shapeWidth = 20, shapeHeight = 10 }),
         (T.pack "h6", []), "no intersection for hybrid"),
        ((7, [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2], defaultRect { shapePos = (199.8863, 88.1213)}),
         (T.pack "csc108csc148", [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2]), "multiple text intersections for node"),
        ((8, [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2], defaultRect { shapePos = (0, 0), shapeType_ = Hybrid, shapeWidth = 202, shapeHeight = 202 }),
         (T.pack "h8", [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2]), "multiple text intersections for hybrid")
    ]

-- Test cases for buildRect with translation.
buildRectTranslationInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildRectTranslationInputs = [
        ((1, [defaultRectText, defaultRectText2], defaultRect { shapePos = (0.0, 100.0), shapeTransform = [1,0,0,1,50,0], shapeType_ = Hybrid }),
         (T.pack "h1", [defaultRectText]), "translate x"),
        ((2, [defaultRectText, defaultRectText2], defaultRect { shapeTransform = [1,0,0,1,0,-30] }),
         (T.pack "csc108", [defaultRectText]), "translate y"),
        ((3, [defaultRectText, defaultRectText2], defaultRect { shapePos = (50.0, 80.0), shapeTransform = [1,0,0,1,-40,15] }),
         (T.pack "csc108", [defaultRectText]), "translate xy"),
        ((4, [defaultRectText, defaultRectText2], defaultRect { shapeTransform = [1,0,0,1,1,1] }),
         (T.pack "", []), "no intersection"),
        ((5, [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2], defaultRect { shapePos = (0.0, 0.0), shapeTransform = [1,0,0,1,200,89], shapeWidth = 202, shapeHeight = 202, shapeType_ = Hybrid }),
         (T.pack "h5", [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2]), "multiple texts")
    ]

-- Test cases for buildRect with scaling.
buildRectScaleInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildRectScaleInputs = [
        ((1, [defaultRectText, defaultRectText2], defaultRect { shapePos = (1.0, 100.0), shapeTransform = [50,0,0,1,0,0], shapeWidth = 10, shapeHeight = 10 }),
         (T.pack "csc108", [defaultRectText]), "scale x"),
        ((2, [defaultRectText, defaultRectText2], defaultRect { shapePos = (50.0, 1.0), shapeTransform = [1,0,0,100,0,0], shapeWidth = 10, shapeHeight = 10 }),
         (T.pack "csc108", [defaultRectText]), "scale y"),
        ((3, [defaultRectText, defaultRectText2], defaultRect { shapePos = (1.0, 1.0), shapeTransform = [49,0,0,99,0,0], shapeWidth = 10, shapeHeight = 10 }),
         (T.pack "csc108", [defaultRectText]), "scale xy"),
        ((4, [defaultRectText, defaultRectText2], defaultRect { shapeTransform = [-1,0,0,1,0,0] }),
         (T.pack "", []), "reflect x, no intersection"),
        ((5, [defaultRectText, defaultRectText2], defaultRect { shapeTransform = [0,0,0,-1,0,0] }),
         (T.pack "", []), "reflect y, no intersection"),
        ((6, [defaultRectText, defaultRectText2], defaultRect { shapePos = (80.0, 101.56), shapeTransform = [-0.1,0,0,0.9,0,0] }),
         (T.pack "", []), "reflect xy"),
        ((7, [defaultRectText, defaultRectText2], defaultRect { shapeTransform = [0.1,0,0,1.5,0,0] }),
         (T.pack "", []), "no intersection"),
        ((8, [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2], defaultRect { shapePos = (1.0, 1.0), shapeTransform = [199,0,0,88,0,0], shapeWidth = 10, shapeHeight = 10 }),
         (T.pack "csc108csc148", [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2]), "multiple texts"),
        ((9, [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2], defaultRect { shapePos = (0.0, 0.0), shapeTransform = [100,0,0,100,0,0], shapeWidth = 10, shapeHeight = 10 }),
         (T.pack "csc108csc148", [defaultRectText { textPos = (200.9999, 89.99997) }, defaultRectText2]), "on (0,0)")
    ]

-- Test cases for buildRect with rotation/skewing.
buildRectShearInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildRectShearInputs = [
        ((1, [defaultRectText { textTransform = [0,1,-1,0,0,0] }], defaultRect { shapePos = (40.0, 90.0), shapeTransform = [0,1,-1,0,0,0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [0,1,-1,0,0,0] }]), "CW rotation"),
        ((2, [defaultRectText { textTransform = [0,-1,1,0,0,0] }], defaultRect { shapePos = (40.0, 90.0), shapeTransform = [0,-1,1,0,0,0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [0,-1,1,0,0,0] }]), "CWW rotation"),
        ((3, [defaultRectText { textTransform = [1,0.5,0,1,0,0] }], defaultRect { shapePos = (40.0, 90.0), shapeTransform = [1,0.6,0,1,0,0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [1,0.5,0,1,0,0] }]), "skew x"),
        ((4, [defaultRectText { textTransform = [1,0,-1.2,1,0,0] }], defaultRect { shapePos = (40.0, 90.0), shapeTransform = [1,0,-1.2,1,0,0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [1,0,-1.2,1,0,0] }]), "skew y"),
        ((5, [defaultRectText { textTransform = [1.1,0.5,1.2,1.1,0,0] }], defaultRect { shapePos = (40.0, 90.0), shapeTransform = [1.1,0.5,1.2,1.05,0,0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [1.1,0.5,1.2,1.1,0,0] }]), "skew xy")
    ]

-- Test cases for buildRect with a mixture of different transformations.
buildRectMixedInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildRectMixedInputs = [
        -- ((textMocks, 1, setTransformation (head rectMocks) [1.5,0,0,1.5,-30,-60]), (T.pack "csc108", [head textMocks])),
        -- ((textMocks, 2, setTransformation (rectMocks !! 2) [-1,0,0,1.2,50,-20]), (T.pack "csc108", [head textMocks])),
        -- ((textMocks, 3, setTransformation (head rectMocks) [1,0,0,-0.8,100,158]), (T.pack "csc148csc111", [textMocks !! 1, textMocks !! 2])),
        -- ((textMocks, 4, setTransformation (rectMocks !! 4) [2.5,0,0,3.5,5,5]), (T.pack "", []))
    ]


-- Test cases for buildEllipses with no transformations.
buildEllipsesNoTransformationInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildEllipsesNoTransformationInputs = [
        ((1, [defaultEllipseText, defaultEllipseText2], defaultEllipse),
         (T.pack "bool1", [defaultEllipseText]), "within the region, i.e. calulation in intersectsEllipse < 1"),
        ((2, [defaultEllipseText { textPos = (98.0, 90.0) }, defaultEllipseText2], defaultEllipse { shapePos = (100.0, 100.0), shapeWidth = 24 }),
         (T.pack "bool2", []), "on the border, i.e. calculation in intersectsEllipse == 1"),
        ((3, [defaultEllipseText { textPos = (210.0, 205.99) }, defaultEllipseText2],  defaultEllipse { shapePos = (200.5, 200.5) }),
         (T.pack "bool3", []), "outside the region, i.e. calculation in intersectsEllipse > 1"),
        ((4, [defaultEllipseText { textPos = (300.0, 300.0) }, defaultEllipseText2], defaultEllipse { shapePos = (300.99, 300.51) }),
         (T.pack "bool4", [defaultEllipseText { textPos = (300.0, 300.0) }, defaultEllipseText2]), "multiple texts within the region")
    ]

-- Test cases for buildEllipses with translation.
buildEllipsesTranslationInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildEllipsesTranslationInputs = [
        ((1, [defaultEllipseText, defaultEllipseText2], defaultEllipse { shapeTransform = [1,0,0,1,10,0] }),
         (T.pack "bool1", [defaultEllipseText]), "translate x"),
        ((2, [defaultEllipseText, defaultEllipseText2], defaultEllipse { shapeTransform = [1,0,0,1,0,-10] }),
         (T.pack "bool2", []), "translate y, no intersection"),
        ((3, [defaultEllipseText { textPos = (300.0, 300.0) }, defaultEllipseText2], defaultEllipse { shapeTransform = [1,0,0,1,300,300] }),
         (T.pack "bool3", [defaultEllipseText { textPos = (300.0, 300.0) }, defaultEllipseText2]), "translate xy, multiple texts")
    ]

-- Test cases for buildEllipses with scaling.
buildEllipsesScaleInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildEllipsesScaleInputs = [
        ((1, [defaultEllipseText { textPos = (210.0, 205.99) }], defaultEllipse { shapePos = (200.5, 200.5), shapeTransform = [1.1,0,0,1,0,0] }),
         (T.pack "bool1", [defaultEllipseText { textPos = (210.0, 205.99) }]), "scale x"),
        ((2, [defaultEllipseText { textPos = (210.0, 205.99) }], defaultEllipse { shapePos = (200.5, 200.5), shapeTransform = [1.1,0,0,1,0,0] }),
         (T.pack "bool2", [defaultEllipseText { textPos = (210.0, 205.99) }]), "scale y"),
        ((3, [defaultEllipseText { textPos = (410.0, 410.0) }], defaultEllipse { shapePos = (300.99, 300.51), shapeTransform = [1.39,0,0,1.39,0,0] }),
         (T.pack "bool3", [defaultEllipseText { textPos = (410.0, 410.0) }]), "scale xy"),
        ((4, [defaultEllipseText { textPos = (300.0, 300.0) }], defaultEllipse { shapePos = (300.99, 300.51), shapeTransform = [-1,0,0,1,0,0] }),
         (T.pack "bool4", []), "reflect x"),
        ((5, [defaultEllipseText, defaultEllipseText2], defaultEllipse { shapePos = (300.99, 300.51), shapeTransform = [1,0,0,-0.01,0,0] }),
         (T.pack "bool5", []), "reflect y"),
        ((6, [defaultEllipseText, defaultEllipseText2], defaultEllipse { shapePos = (300.99, 300.51), shapeTransform = [-0.2,0,0,-200,0,0] }),
         (T.pack "bool6", []), "reflect xy"),
        ((7, [defaultEllipseText, defaultEllipseText2], defaultEllipse { shapeTransform = [1000,0,0,1000,0,0] }),
         (T.pack "bool7", [defaultEllipseText, defaultEllipseText2]), "big scale")
    ]


-- TODO: add tests for text transformations on either buildRect or buildEllipse


-- * Helpers

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
                 -> ((Integer, [Text], Shape), (T.Text, [Text]), String)
                 -> Test
testShapeBuilder fn label shapeLabel input =
    let ((elementId, texts, rect), (expectedId_, expectedTexts), testLabel) = input
        result = fn texts rect elementId
    in TestLabel (label ++ testLabel) $ TestCase $ do
        assertEqual ("Check id_ failed for " ++ shapeLabel ++ " " ++ show elementId) expectedId_ $ shapeId_ result
        assertBool ("Check texts failed for " ++ shapeLabel ++ " " ++ show elementId) $ compareTexts expectedTexts $ shapeText result

-- Run all test cases for buildRect
runBuildRectTests :: [Test]
runBuildRectTests =
    map (testShapeBuilder buildRect "Test buildRect no transformation: " "rect") buildRectNoTransformInputs ++
    map (testShapeBuilder buildRect "Test buildRect translation: " "rect") buildRectTranslationInputs ++
    map (testShapeBuilder buildRect "Test buildRect scaling: " "rect") buildRectScaleInputs ++
    map (testShapeBuilder buildRect "Test buildRect rotation/skewing: " "rect") buildRectShearInputs ++
    map (testShapeBuilder buildRect "Test buildRect mixed transformations: " "rect") buildRectMixedInputs

-- Run all test cases for buildEllipses
runBuildEllipsesTests :: [Test]
runBuildEllipsesTests =
    map (testShapeBuilder buildEllipses "Test buildEllipses no transformation: " "ellipse") buildEllipsesNoTransformationInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses translation: " "ellipse") buildEllipsesTranslationInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses scaling: " "ellipse") buildEllipsesScaleInputs


-- Test suite for intersection checks
intersectionTestSuite :: Test
intersectionTestSuite = TestLabel "Intersection tests" $
    TestList $ runBuildRectTests ++ runBuildEllipsesTests
