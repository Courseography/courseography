{-|
Description: Test entity intersections.

Module that contains the tests for intersection checks in the SVG Builder module.

-}

module SvgTests.IntersectionTests (test_intersections) where

import qualified Data.Text as T
import Database.DataType (ShapeType (..))
import Database.Persist.Sqlite (toSqlKey)
import Database.Tables (Path (..), Shape (..), Text (..))
import Svg.Builder (buildEllipses, buildPath, buildRect, intersectsWithShape)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

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
        ((3, [defaultRectText { textTransform = [1,0,-1.2,1,0,0] }], defaultRect { shapePos = (40.0, 90.0), shapeTransform = [1,0,-1.2,1,0,0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [1,0,-1.2,1,0,0] }]), "skew x"),
        ((4, [defaultRectText { textTransform = [1,0.5,0,1,0,0] }], defaultRect { shapePos = (40.0, 90.0), shapeTransform = [1,0.6,0,1,0,0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [1,0.5,0,1,0,0] }]), "skew y"),
        ((5, [defaultRectText { textTransform = [1.1,0.5,1.2,1.1,0,0] }], defaultRect { shapePos = (40.0, 90.0), shapeTransform = [1.05,0.5,1.2,1.1,0,0] }),
         (T.pack "csc108", [defaultRectText { textTransform = [1.1,0.5,1.2,1.1,0,0] }]), "skew xy")
    ]

-- Test cases for buildRect with a mixture of different transformations.
buildRectMixedInputs :: [((Integer, [Text], Shape), (T.Text, [Text]), String)]
buildRectMixedInputs = [
        ((1, [defaultRectText { textTransform = [-15.5, 0.3, -0.2, 12, 500, 3000] }, defaultRectText2], defaultRect { shapeTransform = [-15.5, 0.3, -0.2, 12, 500, 3000] }),
         (T.pack "", []), "complex transformation where texts and shape has the same matrices (should have no intersections due to floating point errors)"),
        ((2, [defaultRectText { textTransform = [1.5, 0.1, 0.1, 1.5, -3.33, 3.33] }, defaultRectText2], defaultRect { shapeTransform = [1.49, 0.1, 0.105, 1.5, -3.33, 3.33] }),
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
        ((2, [defaultEllipseText { textTransform = [0.707, -0.707, 0.707, 0.707, 0, 0] }], defaultEllipse { shapeTransform = [0.707, -0.707, 0.72, 0.707, 0, 0] }),
         (T.pack "bool2", [defaultEllipseText { textTransform = [0.707, -0.707, 0.707, 0.707, 0, 0] }]), "CCW rotation"),
        ((3, [defaultEllipseText { textTransform = [1,0,4.5,1,0,0] }], defaultEllipse { shapeTransform = [1,0,4.5,1,0,0] }),
         (T.pack "bool3", [defaultEllipseText { textTransform = [1,0,4.5,1,0,0] }]), "skew x"),
        ((4, [defaultEllipseText { textTransform = [1,0.5,0,1,0,0] }], defaultEllipse { shapeTransform = [1,0.5,0,1,0,0] }),
         (T.pack "bool4", [defaultEllipseText { textTransform = [1,0.5,0,1,0,0] }]), "skew y"),
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

-- Test cases for intersectsWithShape with translation.
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
        ((5, defaultEllipseText { textPos = (20.99, 10.0), textTransform = [1,0,0,1,0,-10.0] },
         [defaultEllipse { shapePos = (0.0, 0.0) }]),
         True, "translate y for ellipse"),
        ((6, defaultEllipseText { textPos = (45.1, 40.1) },
         [defaultEllipse { shapePos = (0.0, 0.0), shapeTransform = [1,0,0,1,12.5,10.0] }, defaultEllipse { shapePos = (1000, 1000) }]),
         False, "translate xy for ellipse")
    ]

-- Test cases for intersectsWithShape with scaling.
intersectsWithShapeScaleInputs :: [((Integer, Text, [Shape]), Bool, String)]
intersectsWithShapeScaleInputs = [
        ((1, defaultRectText { textPos = (200.55, 1.0) },
         [defaultRect { shapePos = (1.0, 1.0), shapeTransform = [2,0,0,1,0,0] }]),
         False, "scale x for rect"),
        ((2, defaultEllipseText { textPos = (0.0, 100.0), textTransform = [1,0,0,0.5,0,0] },
         [defaultEllipse { shapePos = (0.0, 50.0) }]),
         True, "scale y for ellipse"),
        ((3, defaultRectText { textPos = (0.0, 99.99) },
         [defaultRect { shapePos = (0.0, 40.0), shapeTransform = [100000,0,0,2.5,0,0] }]),
         True, "scale xy for rect where shape pos is (0,0)"),
        ((4, defaultEllipseText { textPos = (0.0, 0.0), textTransform = [-1,0,0,1,0,0] },
         [defaultEllipse { shapePos = (0.0, 50.0) }]),
         False, "reflect x for ellipse"),
        ((5, defaultRectText { textPos = (10.0, 10), textTransform = [1,0,0,-0.5,0,0] },
         [defaultRect { shapePos = (0.0, 10.0), shapeTransform = [1,0,0,-0.5,0,0] }]),
         True, "reflect y for rect"),
        ((6, defaultEllipseText { textPos = (0.0, 0.0), textTransform = [-10,0,0,-10,0,0] },
         [defaultEllipse { shapePos = (1.0, 1.0), shapeTransform = [2,0,0,1.1,0,0] }]),
         True, "reflect x for ellipse where text pos is (0,0)")
    ]

-- Test cases for intersectsWithShape with rotation/skewing.
intersectsWithShapeShearInputs :: [((Integer, Text, [Shape]), Bool, String)]
intersectsWithShapeShearInputs = [
        ((1, defaultRectText { textTransform = [0,1,-1,0,0,0] },
         [defaultRect { shapeTransform = [0,1,-1,0,0,0] }]),
         True, "CW rotation for rect"),
        ((2, defaultRectText,
         [defaultRect { shapeTransform = [0,-1,1,0,0,0] }]),
         False, "CCW rotation for rect"),
        ((3, defaultEllipseText { textPos = (1.0, 10.0) },
         [defaultEllipse { shapeTransform = [0,0.5,-0.5,0,0,0] }]),
         True, "CW rotation for ellipse"),
        ((4, defaultEllipseText { textPos = (50.0, 50.0) },
         [defaultEllipse { shapePos = (40.0, 40.0), shapeTransform = [0,-0.5,0.5,0,0,0] }]),
         False, "CWW rotation for ellipse"),
        ((5, defaultRectText { textPos = (100.0, 100.0) },
         [defaultRect { shapePos = (176.0, 100.0), shapeTransform = [1,0,-0.76,1,0,0]}]),
         True, "skew x for rect"),
        ((6, defaultRectText { textPos = (35.0, 30.98) },
         [defaultRect { shapePos = (29.64, 0.0), shapeTransform = [1,1,0,1,0,0]}]),
         True, "skew y for rect"),
        ((7, defaultRectText { textPos = (100.0, 100.0) },
         [defaultRect { shapePos = (2.0, 46.0), shapeTransform = [6.35,-1.82,1.85,2.2,0,0]}]),
         True, "skew xy for rect"),
        ((8, defaultEllipseText { textPos = (100.0, 100.0) },
         [defaultEllipse { shapePos = (100.0, 100.0), shapeTransform = [1,0,10,1,0,0] }]),
         False, "skew x for ellipse"),
        ((9, defaultEllipseText { textPos = (100.0, 100.0) },
         [defaultEllipse { shapePos = (100.0, 100.0), shapeTransform = [1,-0.1,0,1,0,0] }]),
         True, "skew y for ellipse"),
        ((10, defaultEllipseText { textPos = (40.0, 40.0) },
         [defaultEllipse { shapePos = (13.87, 43.22), shapeTransform = [1.2,0.5,-1,1.2,0,0] }]),
         True, "skew xy for ellipse")
    ]

-- Test cases for intersectsWithShape with a mixture of different transformations.
intersectsWithShapeMixedInputs :: [((Integer, Text, [Shape]), Bool, String)]
intersectsWithShapeMixedInputs = [
        ((1, defaultEllipseText { textPos = (100.0, 100.0), textTransform = [9.8, -23, 0.11, -3.2, 1333, -302] },
         [defaultEllipse { shapePos = (101.0, 101.0), shapeTransform = [9.8, -23, 0.11, -3.2, 1333, -302] }, defaultRect { shapePos = (102.0, 102.0), shapeTransform = [9.8, -23, 0.11, -3.2, 1333, -302] }]),
         True, "complex transformation for both rects and ellipses, same transformation for all entities"),
        ((2, defaultRectText { textPos = (100.0, 100.0), textTransform = [1, 0.1, 0.1, 2, 50, 50] },
         [defaultEllipse { shapePos = (100.0, 100.0), shapeTransform = [1, 0.1, 0.1, 2, 50, 50] }, defaultRect { shapePos = (100.0, 100.0), shapeTransform = [1, 0.1, 0.1, 2, 50, 50] }]),
         True, "complex transformation for both rects and ellipses, different transformation for all entities"),
        ((3, defaultEllipseText { textPos = (50.11, 49.11) },
         [defaultEllipse { shapePos = (50.11, 49.11), shapeTransform = [2, 0.9, -1.2, 1.5, 15, 9.45] }]),
         False, "complex transformation, intersects at raw point but not when transformed"),
        ((4, defaultRectText { textPos = (100.0, 100.0) },
         [defaultEllipse, defaultRect { shapePos = (1.5, 40.99), shapeTransform = [-0.53, 0.85, 2.45, 1.91, -0.15, 5.29] }]),
         True, "complex transformation, doesn't intersect at raw point but do intersect when transformed"),
        ((5, defaultRectText { textPos = (100.0, 100.0) },
         [defaultRect { shapePos = (5.0, 5.0), shapeTransform = [-1, -0.2, -2, 0.5, -2, 20] }, defaultEllipse]),
         False, "complex transformation, doesn't intersect at raw point and after transformed"),
        ((6, defaultRectText,
         []),
         False, "no shapes to check")
    ]


-- * buildPath tests

-- Test cases for buildPath with no transformations.
buildPathNoTransformationInputs :: [((Integer, Path, [Shape], [Shape]), (T.Text, T.Text, T.Text), String)]
buildPathNoTransformationInputs = [
        ((1, defaultPath,
         [defaultRect { shapePos = (0.0, 0.0), shapeId_ = T.pack "csc108" }, defaultRect { shapePos = (100.0, 100.0), shapeId_ = T.pack "csc148" }], [defaultEllipse]),
         (T.pack "p1", T.pack "csc108", T.pack "csc148"), "path intersects two rects at source and target"),
        ((2, defaultPath,
         [defaultRect { shapePos = (0.0, 0.0), shapeId_ = T.pack "csc108" } ], [defaultEllipse { shapePos = (90.0, 90.0), shapeId_ = T.pack "ellipse1" }]),
         (T.pack "p2", T.pack "csc108", T.pack "ellipse1"), "path intersects rect at source and ellipse at target"),
        ((3, defaultPath,
         [defaultRect { shapePos = (110.0, 110.0), shapeId_ = T.pack "csc108" } ], [defaultEllipse { shapePos = (1.0, 1.0), shapeId_ = T.pack "ellipse1" }]),
         (T.pack "p3", T.pack "ellipse1", T.pack ""), "path only intersects with ellipse at source"),
        ((4, defaultPath,
         [defaultRect { shapePos = (70.0, 70.0), shapeId_ = T.pack "csc108" } ], [defaultEllipse { shapePos = (50.99, 50.19), shapeId_ = T.pack "ellipse1" }]),
         (T.pack "p4", T.pack "", T.pack "csc108"), "path only intersects with rect at target"),
        ((5, defaultPath,
         [defaultRect { shapePos = (50.0, 50.0) }], [defaultEllipse { shapePos = (50.0, 50.0) }]),
         (T.pack "p5", T.pack "", T.pack ""), "path doesn't intersect with any shape"),
        ((6, defaultPath,
         [defaultRect { shapePos = (0.0, 0.0), shapeId_ = T.pack "csc108" }], [defaultEllipse { shapePos = (0.0, 0.0), shapeId_ = "ellipse1" }]),
         (T.pack "p6", T.pack "csc108", T.pack ""), "path intersects with multiple shapes")
    ]

-- Test cases for buildPath with translation.
buildPathTranslationInputs :: [((Integer, Path, [Shape], [Shape]), (T.Text, T.Text, T.Text), String)]
buildPathTranslationInputs = [
        ((1, defaultPath { pathTransform = [1,0,0,1,50,0] },
         [defaultRect { shapePos = (0.0, 0.0), shapeId_ = T.pack "csc108", shapeWidth = 10 }, defaultRect { shapePos = (100.0, 100.0), shapeId_ = T.pack "csc148", shapeWidth = 10 }], []),
         (T.pack "p1", T.pack "", T.pack ""), "translate x for path"),
        ((2, defaultPath { pathTransform = [1,0,0,1,0,20] },
         [defaultRect { shapePos = (0.0, 0.0), shapeId_ = T.pack "csc108" }], [defaultEllipse { shapePos = (100.0, 120.0), shapeTransform = [1,0,0,1,0,-10], shapeId_ = T.pack "ellipse1" }]),
         (T.pack "p2", T.pack "csc108", T.pack "ellipse1"), "translate y for path"),
        ((3, defaultPath { pathTransform = [1,0,0,1,20,20] },
         [defaultRect { shapePos = (0.0, 0.0), shapeId_ = T.pack "csc108" }, defaultRect { shapePos = (100.0, 100.0), shapeTransform = [1,0,0,1,-5,6.7], shapeId_ = T.pack "csc148" }], [defaultEllipse]),
         (T.pack "p3", T.pack "csc108", T.pack "csc148"), "translate xy for path")
    ]

-- Test cases for intersectsWithShape with scaling.
buildPathScaleInputs :: [((Integer, Path, [Shape], [Shape]), (T.Text, T.Text, T.Text), String)]
buildPathScaleInputs = [
        ((1, defaultPath { pathTransform = [1.1,0,0,1,0,0] },
         [defaultRect { shapePos = (100.0, 100.0), shapeId_ = T.pack "csc108"}], [defaultEllipse { shapePos = (0.0, 0.0), shapeId_ = "ellipse1" }]),
         (T.pack "p1", T.pack "ellipse1", T.pack "csc108"), "scale x for path"),
        ((2, defaultPath { pathTransform = [1,0,0,0.75,0,0] },
         [], [defaultEllipse { shapePos = (100.0, 60.0), shapeTransform = [1,0,0,1.1,0,0], shapeId_ = T.pack "ellipse1" }]),
         (T.pack "p2", T.pack "", T.pack "ellipse1"), "scale y for path and shape"),
        ((3, defaultPath { pathPoints = [(1.0, 1.0), (100.0, 100.0)], pathTransform = [0.7,0,0,2,0,0] },
         [defaultRect { shapePos = (1.0, 1.0), shapeId_ = "csc108", shapeTransform = [1000,0,0,1,0,0] }], [defaultEllipse { shapePos = (77.11, 180.976), shapeTransform = [0.9,0,0,1.1,0,0], shapeId_ = T.pack "ellipse1" }]),
         (T.pack "p3", T.pack "csc108", T.pack "ellipse1"), "scale xy for path and shape"),
        ((4, defaultPath { pathTransform = [-1,0,0,1,0,0] },
         [defaultRect { shapePos = (100.0, 100.0), shapeId_ = T.pack "csc108"}], [defaultEllipse { shapePos = (0.0, 0.0), shapeId_ = "ellipse1" }]),
         (T.pack "p4", T.pack "ellipse1", T.pack ""), "reflect x for path"),
        ((5, defaultPath { pathTransform = [1,0,0,-0.75,0,0] },
         [defaultRect { shapePos = (100.0, 70.0), shapeTransform = [1,0,0,-1,0,0], shapeId_ = T.pack "csc108" }], []),
         (T.pack "p5", T.pack "", T.pack "csc108"), "reflect y for path and shape"),
        ((6, defaultPath { pathPoints = [(1.0, 1.0), (100.0, 100.0)], pathTransform = [-0.2,0,0,-0.8,0,0] },
         [defaultRect { shapePos = (1.0, 1.0), shapeId_ = "csc108", shapeTransform = [-0.2,0,0,-0.8,0,0] }], [defaultEllipse { shapePos = (100.0, 100.0), shapeTransform = [-0.2,0,0,-0.8,0,0], shapeId_ = T.pack "ellipse1" }]),
         (T.pack "p6", T.pack "csc108", T.pack "ellipse1"), "reflect xy for path and shape")
    ]

-- Test cases for intersectsWithShape with rotation/skewing.
buildPathShearInputs :: [((Integer, Path, [Shape], [Shape]), (T.Text, T.Text, T.Text), String)]
buildPathShearInputs = [
        ((1, defaultPath { pathPoints = [(100.0, 100.0), (500.0, 500.0)], pathTransform = [0.5,0.866,-0.866,0.5,0,0] },
         [defaultRect { shapePos = (101.0, 98.23), shapeTransform = [0.5,0.866,-0.866,0.5,0,0], shapeId_ = T.pack "csc108" }], [defaultEllipse { shapePos = (494.112, 500.54), shapeTransform = [0.5,0.866,-0.866,0.5,0,0], shapeId_ = T.pack "ellipse1"}]),
         (T.pack "p1", T.pack "csc108", T.pack "ellipse1"), "CW rotation"),
        ((2, defaultPath { pathPoints = [(100.0, 100.0), (500.0, 500.0)], pathTransform = [0.5,-0.866,0.866,0.5,0,0] },
         [defaultRect { shapePos = (101.0, 98.23), shapeTransform = [0.5,0.866,-0.866,0.5,0,0], shapeId_ = T.pack "csc108" }], [defaultEllipse { shapePos = (494.112, 500.54), shapeTransform = [0.5,-0.866,0.866,0.5,0,0], shapeId_ = T.pack "ellipse1"}]),
         (T.pack "p2", T.pack "", T.pack "ellipse1"), "CCW rotation"),
        ((3, defaultPath { pathTransform = [1,0,0.688,1,0,0] },
         [defaultRect { shapePos = (1.0, 1.23), shapeTransform = [1,0,0.6,1,0,0], shapeId_ = T.pack "csc108" }], [defaultEllipse]),
         (T.pack "p3", T.pack "csc108", T.pack ""), "skew x"),
        ((4, defaultPath { pathTransform = [1,0.577,0,1,0,0] },
         [defaultRect { shapePos = (101.0, 98.23), shapeTransform = [1,0.577,0,1,0,0], shapeId_ = T.pack "csc108" }], [defaultEllipse]),
         (T.pack "p4", T.pack "", T.pack "csc108"), "skew y"),
        ((5, defaultPath { pathPoints = [(100.0, 100.0), (500.0, 500.0)], pathTransform = [1,0.577,-0.364,1,0,0] },
         [defaultRect], [defaultEllipse { shapePos = (100.0, 100.23), shapeTransform = [1,0.577,-0.364,1,0,0], shapeId_ = T.pack "ellipse1" }, defaultEllipse { shapePos = (488.0, 499.23), shapeTransform = [1,0.56,-0.354,0.98,0,0], shapeId_ = T.pack "ellipse2" }]),
         (T.pack "p5", T.pack "ellipse1", T.pack "ellipse2"), "skew xy")
    ]

-- Test cases for intersectsWithShape with a mixture of different transformations.
buildPathMixedInputs :: [((Integer, Path, [Shape], [Shape]), (T.Text, T.Text, T.Text), String)]
buildPathMixedInputs = [
        ((1, defaultPath { pathPoints = [(100.0, 100.0), (500.0, 500.0)], pathTransform = [1.5,0.4,-0.6,-0.8,20,-35] },
         [defaultRect { shapePos = (80.993, 80.28), shapeTransform = [1.5,0.4,-0.6,-0.8,20,-35], shapeId_ = T.pack "csc108"}], [defaultEllipse { shapePos = (498.213, 489.092), shapeTransform = [1.5,0.4,-0.6,-0.8,20,-35], shapeId_ = T.pack "ellipse1"}]),
         (T.pack "p1", T.pack "csc108", T.pack "ellipse1"), "complex transformation where all entities has the same transformation"),
        ((2, defaultPath { pathPoints = [(100.0, 100.0), (500.0, 500.0)], pathTransform = [-1.2,-0.3,0.7,0.9,-15,50] },
         [defaultRect { shapePos = (498.063, 470.32), shapeTransform = [-1.2,-0.28,0.69,0.88,-15,45], shapeId_ = T.pack "csc108"}], [defaultEllipse { shapePos = (82.492, 111.0), shapeTransform = [-1.3,-0.39,0.68,0.91,-10,50], shapeId_ = T.pack "ellipse1"}]),
         (T.pack "p2", T.pack "ellipse1", T.pack "csc108"), "complex transformation where entities have different transformations")
    ]


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
                 -> TestTree
testShapeBuilder fn label shapeLabel input =
    let ((elementId, texts, rect), (expectedId_, expectedTexts), testLabel) = input
        result = fn texts rect elementId
    in testCase (label ++ testLabel) $ do
        assertEqual ("Check id_ failed for " ++ shapeLabel ++ " " ++ show elementId) expectedId_ $ shapeId_ result
        assertBool ("Check texts failed for " ++ shapeLabel ++ " " ++ show elementId) $ compareTexts expectedTexts $ shapeText result

-- Function for testing a intersectsWithShape test case
testIntersectsWithShape :: String
                        -> ((Integer, Text, [Shape]), Bool, String)
                        -> TestTree
testIntersectsWithShape label input =
    let ((testId, text, shapes), expected, testLabel) = input
        actual = intersectsWithShape shapes text
    in testCase (label ++ testLabel) $ do
        assertEqual ("Check intersection failed for case " ++ show testId) expected actual

-- Function for testing a buildPath test case
testBuildPath :: String
              -> ((Integer, Path, [Shape], [Shape]), (T.Text, T.Text, T.Text), String)
              -> TestTree
testBuildPath label input =
    let ((elementId, path, rects, ellipses), (expectedId_, expectedSource, expectedTarget), testLabel) = input
        result = buildPath rects ellipses path elementId
    in testCase (label ++ testLabel) $ do
        assertEqual ("Check id_ failed for path " ++ show elementId) expectedId_ $ pathId_ result
        assertEqual ("Check source node failed for path " ++ show elementId) expectedSource $ pathSource result
        assertEqual ("Check target node failed for path " ++ show elementId) expectedTarget $ pathTarget result


-- Run all test cases for buildRect
runBuildRectTests :: [TestTree]
runBuildRectTests =
    map (testShapeBuilder buildRect "Test buildRect no transformation: " "rect") buildRectNoTransformInputs ++
    map (testShapeBuilder buildRect "Test buildRect translation: " "rect") buildRectTranslationInputs ++
    map (testShapeBuilder buildRect "Test buildRect scaling: " "rect") buildRectScaleInputs ++
    map (testShapeBuilder buildRect "Test buildRect rotation/skewing: " "rect") buildRectShearInputs ++
    map (testShapeBuilder buildRect "Test buildRect mixed transformations: " "rect") buildRectMixedInputs

-- Run all test cases for buildEllipses
runBuildEllipsesTests :: [TestTree]
runBuildEllipsesTests =
    map (testShapeBuilder buildEllipses "Test buildEllipses no transformation: " "ellipse") buildEllipsesNoTransformationInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses translation: " "ellipse") buildEllipsesTranslationInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses scaling: " "ellipse") buildEllipsesScaleInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses rotation/skewing: " "ellipse") buildEllipsesShearInputs ++
    map (testShapeBuilder buildEllipses "Test buildEllipses mixed transformations: " "ellipse") buildEllipsesMixedInputs

-- Run all test cases for buildPath
runIntersectsWithShape :: [TestTree]
runIntersectsWithShape =
    map (testIntersectsWithShape "Test intersectsWithShape no transformation: ") intersectsWithShapeNoTransformationInputs ++
    map (testIntersectsWithShape "Test intersectsWithShape translation: ") intersectsWithShapeTranslationInputs ++
    map (testIntersectsWithShape "Test intersectsWithShape scaling: ") intersectsWithShapeScaleInputs ++
    map (testIntersectsWithShape "Test intersectsWithShape rotation/skewing: ") intersectsWithShapeShearInputs ++
    map (testIntersectsWithShape "Test intersectsWithShape mixed transformations: ") intersectsWithShapeMixedInputs

-- Run all test cases for buildPath
runBuildPathTests :: [TestTree]
runBuildPathTests =
    map (testBuildPath "Test buildPath no transformation: ") buildPathNoTransformationInputs ++
    map (testBuildPath "Test buildPath translation: ") buildPathTranslationInputs ++
    map (testBuildPath "Test buildPath scaling: ") buildPathScaleInputs ++
    map (testBuildPath "Test buildPath rotation/skewing: ") buildPathShearInputs ++
    map (testBuildPath "Test buildPath mixed transformations: ") buildPathMixedInputs


-- Test suite for intersection checks
test_intersections :: TestTree
test_intersections = testGroup "Intersection tests" $
    runBuildRectTests ++ runBuildEllipsesTests ++ runIntersectsWithShape ++ runBuildPathTests
