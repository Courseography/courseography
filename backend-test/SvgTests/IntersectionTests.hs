{-|
Description: Test entity intersections.

Module that contains the tests for intersection checks in the SVG Builder module.

-}

module SvgTests.IntersectionTests (intersectionTestSuite) where

import Svg.Builder (buildRect, buildEllipses, buildPath, intersectsWithShape)
import Database.Tables (Text(..), Shape(..), Path(..))
import Database.DataType (ShapeType(..))
import qualified Data.Text as T
import Test.HUnit (Test(..), assertEqual, assertBool)
import Database.Persist.Sqlite (toSqlKey)

-- * Mocks

defaultRectText :: Text
defaultRectText = Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (50.0, 100.0), textText = T.pack "CSC108", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0] }

defaultRectText2 :: Text
defaultRectText2 = Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (201.92939, 90.8812), textText = T.pack "CSC148", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0] }

defaultEllipseText :: Text
defaultEllipseText = Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (0.0, 0.0), textText = T.pack "and", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0] }

defaultEllipseText2 :: Text
defaultEllipseText2 = Text { textGraph = toSqlKey 1, textRId = T.pack "", textPos = (301.0, 301.0), textText = T.pack "or", textAlign = T.pack "", textFill = T.pack "", textTransform = [1,0,0,1,0,0] }

defaultRect :: Shape
defaultRect = Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (50.0, 100.0), shapeWidth = 85, shapeHeight = 30, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = Node, shapeTransform = [1,0,0,1,0,0] }

defaultEllipse :: Shape
defaultEllipse = Shape { shapeGraph = toSqlKey 1, shapeId_ = T.pack "", shapePos = (0.0, 0.0), shapeWidth = 25, shapeHeight = 20, shapeFill = T.pack "", shapeStroke = T.pack "", shapeText = [], shapeType_ = BoolNode, shapeTransform = [1,0,0,1,0,0] }

defaultPath :: Path
defaultPath = Path { pathGraph = toSqlKey 1, pathId_ = T.pack "", pathPoints = [(0.0, 0.0), (100.0, 100.0)], pathFill = T.pack "", pathStroke = T.pack "", pathIsRegion = False, pathSource = T.pack "", pathTarget = T.pack "", pathTransform = [1,0,0,1,0,0] }


-- * Test Cases
-- * The first element in the tuple is the inputs, the second is the expected ouput, last is the test case description.

-- * buildRect tests

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
         (T.pack "csc108", [defaultRectText { textTransform = [0,-1,1,0,0,0] }]), "CCW rotation"),
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
        ((1, [defaultRectText { textTransform = [-15.5, 0.3, -0.2, 12, 500, 3000] }, defaultRectText2], defaultRect { shapeTransform = [-15.5, 0.3, -0.2, 12, 500, 3000] }),
         (T.pack "csc108", [defaultRectText { textTransform = [-15.5, 0.3, -0.2, 12, 500, 3000] }]), "complex transformation where texts and shape has the same matrices"),
        ((2, [defaultRectText { textTransform = [1.5, 0.1, 0.1, 1.5, -3.33, 3.33] }, defaultRectText2], defaultRect { shapeTransform = [1.5, 0.1, 0.11, 1.49, -4, 0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [1.5, 0.1, 0.1, 1.5, -3.33, 3.33] }]), "complex transformation where texts and shape has different matrices"),
        ((3, [defaultRectText { textTransform = [1.3, 0.1, 0.1, 1.3, 10, 10] }, defaultRectText2 { textPos = (60.0, 110.0), textTransform = [1.27, 0.1, 0.1, 1.31, 5, 8] }], defaultRect { shapeTransform = [1.27, 0.1, 0.1, 1.31, 5, 8] }),
         (T.pack "csc108csc148", [defaultRectText { textTransform = [1.3, 0.1, 0.1, 1.3, 10, 10] }, defaultRectText2 { textPos = (60.0, 110.0), textTransform = [1.27, 0.1, 0.1, 1.31, 5, 8] }]), "complex transformation with multiple text intersections")
    ]


-- * buildEllipses tests

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

-- Test cases for buildEllipses with rotation/skewing.
buildEllipsesShearInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildEllipsesShearInputs = [
        ((1, [defaultEllipseText { textTransform = [0.707, 0.707, -0.707, 0.707, 0, 0] }], defaultEllipse { shapeTransform = [0.707, 0.707, -0.707, 0.707, 0, 0] }),
         (T.pack "bool1", [defaultEllipseText { textTransform = [0.707, 0.707, -0.707, 0.707, 0, 0] }]), "CW rotation"),
        ((2, [defaultEllipseText { textTransform = [0.707, -0.707, -0.707, 0.707, 0, 0] }], defaultEllipse { shapeTransform = [0.707, -0.707, 0.72, 0.707, 0, 0] }),
         (T.pack "bool2", [defaultEllipseText { textTransform = [0.707, -0.707, -0.707, 0.707, 0, 0] }]), "CCW rotation"),
        ((3, [defaultEllipseText { textTransform = [1,0.5,0,1,0,0] }], defaultEllipse { shapeTransform = [1,0.5,0,1,0,0] }),
         (T.pack "bool3", [defaultEllipseText { textTransform = [1,0.5,0,1,0,0] }]), "skew x"),
        ((4, [defaultEllipseText { textTransform = [1,0,4.5,1,0,0] }], defaultEllipse { shapeTransform = [1,0,4.5,1,0,0] }),
         (T.pack "bool4", [defaultEllipseText { textTransform = [1,0,4.5,1,0,0] }]), "skew y"),
        ((5, [defaultEllipseText { textTransform = [1,-1.5,2.3,1,0,0] }], defaultEllipse { shapeTransform = [1,-1.5,2.3,1,0,0] }),
         (T.pack "bool5", [defaultEllipseText { textTransform = [1,-1.5,2.3,1,0,0] }]), "skew xy")
    ]

-- Test cases for buildEllipses with a mixture of different transformations.
buildEllipsesMixedInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildEllipsesMixedInputs = [
        ((1, [defaultEllipseText { textTransform = [900, -0.000001, 38, 2.1, 500.5, 20.09] }, defaultEllipseText2], defaultEllipse { shapeTransform = [900, -0.000001, 38, 2.1, 500.5, 20.09] }),
         (T.pack "bool1", [defaultEllipseText { textTransform = [900, -0.000001, 38, 2.1, 500.5, 20.09] }]), "complex transformation where texts and shape has the same matrices"),
        ((2, [defaultEllipseText { textTransform = [0.3, 0.05, 3, 0.2, 0, 0] }, defaultEllipseText2], defaultEllipse { shapeTransform = [0.29, 0.04, 3, 0.2, -0.01, -0.01] }),
         (T.pack "bool2", [defaultEllipseText { textTransform = [0.3, 0.05, 3, 0.2, 0, 0] }]), "complex transformation where texts and shape has different matrices"),
        ((3, [defaultEllipseText { textTransform = [0.1, 0.99, 3, 0.2, 2, 5] }, defaultEllipseText2 { textPos = (1.1, 1.1), textTransform = [0.12, 1, 3, 0.19, 1, 0] }], defaultEllipse { shapeTransform = [0.12, 1, 3, 0.19, 1, 0] }),
         (T.pack "bool3", [defaultEllipseText { textTransform = [0.1, 0.99, 3, 0.2, 2, 5] }, defaultEllipseText2 { textPos = (1.1, 1.1), textTransform = [0.12, 1, 3, 0.19, 1, 0] }]), "complex transformation with multiple text intersections")
    ]


-- * intersectsWithShape tests

-- Test cases for intersectsWithShape with no transformations.
intersectsWithShapeNoTransformationInputs :: [((Integer, Text, [Shape]), Bool, String)]
intersectsWithShapeNoTransformationInputs = [
        ((1, defaultRectText { textPos = (1.1, 1.1) },
         [defaultRect { shapePos = (0.0, 0.0) }]),
          True, "within rect"),
        ((2, defaultRectText { textPos = (90.0, 25.0) },
         [defaultRect { shapePos = (0.0, 0.0) } ]),
         True, "within rect shape tolerance (9.0)"),
        ((3, defaultRectText { textPos = (94.0, 39.0) },
         [defaultRect { shapePos = (0.0, 0.0)}]),
         True, "on border of shape tolerance (9.0)"),
        ((4, defaultRectText { textPos = (1000.0, 1000.78) },
         [defaultRect { shapePos = (0.0, 0.0) }, defaultRect { shapePos = (500.0, 500.0) }]),
         False, "not intersecting with rect"),
        ((5, defaultEllipseText { textPos = (2.0, 2.0) },
         [defaultEllipse { shapePos = (0.0, 0.0) }]),
         True, "within ellipse"),
        ((6, defaultEllipseText { textPos = (30.0, 30.0) },
         [defaultEllipse { shapePos = (0.0, 0.0) }]),
         True, "within shape tolerance (20.0)"),
        ((7, defaultEllipseText { textPos = (45.0, 40.0) },
         [defaultEllipse { shapePos = (12.5, 10.0) }]),
         True, "on border of shape tolerance (20.0)"),
        ((8, defaultEllipseText { textPos = (450.0, 400.0) },
         [defaultEllipse { shapePos = (0.0, 0.0) }, defaultEllipse { shapePos = (29.09, 300.23) }]),
         False, "not intersecting with ellipse")
    ]

-- Test cases for buildEllipses with translation.
intersectsWithShapeTranslationInputs :: [((Integer, Text, [Shape]), Bool, String)]
intersectsWithShapeTranslationInputs = [
        ((1, defaultRectText { textPos = (100.0, 0.0) },
         [defaultRect { shapePos = (0.0, 0.0), shapeTransform = [1,0,0,1,100,0] }]),
         True, "translate x for rect"),
        ((2, defaultRectText { textPos = (0.0, 0.0) },
         [defaultRect { shapePos = (0.0, 200.0), shapeTransform = [1,0,0,1,100,-100] }]),
         False, "translate x for rect"),
        ((3, defaultRectText { textPos = (100.0, 0.0), textTransform = [1,0,0,1,-100,10] },
         [defaultRect { shapePos = (0.0, 0.0) , shapeTransform = [1,0,0,1,100,100] }, defaultRect { shapePos = (0.0, 0.0) }]),
         True, "translate xy for rect"),
        ((4, defaultEllipseText { textPos = (50.0, 50.0) },
         [defaultEllipse { shapePos = (0.0, 50.0), shapeTransform = [1,0,0,1,30,0] }]),
         True, "translate xy for ellipse"),
        ((5, defaultEllipseText { textPos = (20.99, 10.0), textTransform = [1,0,0,1,0,-10.0]},
         [defaultEllipse { shapePos = (0.0, 0.0) }]),
         True, "translate y for ellipse"),
        ((6, defaultEllipseText { textPos = (45.1, 40.1) },
         [defaultEllipse { shapePos = (0.0, 0.0), shapeTransform = [1,0,0,1,12.5,10.0] }, defaultEllipse { shapePos = (1000, 1000) }]),
         False, "translate xy for ellipse")
    ]


-- * buildPath tests

-- Test cases for buildPath with no transformations.
buildPathNoTransformationInputs :: [((Integer, Path, [Shape], [Shape]), (T.Text, T.Text, T.Text), String)]
buildPathNoTransformationInputs = [
        ((1, defaultPath, [], []), (T.pack "p1", T.pack "", T.pack ""), "test set up")
    ]

-- TODO test cases for buildPath
-- no need to consider shape tolerance/corner/border cases, covered in tests for intersectsWithShape
-- no need to consider shape type, covered in tests for intersectsWithShape
-- path intersects with node normally
-- no intersections
-- only one intersection (only one shape vs more than one shapes)
-- more than one shape intersects at point
-- diff transformation with shape

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

-- Function for testing a intersectsWithShape test case
testIntersectsWithShape :: String
                        -> ((Integer, Text, [Shape]), Bool, String)
                        -> Test
testIntersectsWithShape label input =
    let ((testId, text, shapes), expected, testLabel) = input
        actual = intersectsWithShape shapes text
    in TestLabel (label ++ testLabel) $ TestCase $ do
        assertEqual ("Check intersection failed for case " ++ show testId) expected actual

-- Function for testing a buildPath test case
testBuildPath :: String
              -> ((Integer, Path, [Shape], [Shape]), (T.Text, T.Text, T.Text), String)
              -> Test
testBuildPath label input =
    let ((elementId, path, rects, ellipses), (expectedId_, expectedSource, expectedTarget), testLabel) = input
        result = buildPath rects ellipses path elementId
    in TestLabel (label ++ testLabel) $ TestCase $ do
        assertEqual ("Check id_ failed for path " ++ show elementId) expectedId_ $ pathId_ result
        assertEqual ("Check source node failed for path " ++ show elementId) expectedSource $ pathSource result
        assertEqual ("Check target node failed for path " ++ show elementId) expectedTarget $ pathTarget result


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
    map (testShapeBuilder buildEllipses "Test buildEllipses scaling: " "ellipse") buildEllipsesScaleInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses rotation/skewing: " "ellipse") buildEllipsesShearInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses mixed transformations: " "ellipse") buildEllipsesMixedInputs

-- Run all test cases for buildPath
runIntersectsWithShape :: [Test]
runIntersectsWithShape =
    map (testIntersectsWithShape "Test intersectsWithShape no transformation: ") intersectsWithShapeNoTransformationInputs ++
    map (testIntersectsWithShape "Test intersectsWithShape translation: ") intersectsWithShapeTranslationInputs

-- Run all test cases for buildPath
runBuildPathTests :: [Test]
runBuildPathTests =
    map (testBuildPath "Test buildPath no transformation: ") buildPathNoTransformationInputs


-- Test suite for intersection checks
intersectionTestSuite :: Test
intersectionTestSuite = TestLabel "Intersection tests" $
    TestList $ runBuildRectTests ++ runBuildEllipsesTests ++ runIntersectsWithShape ++ runBuildPathTests
