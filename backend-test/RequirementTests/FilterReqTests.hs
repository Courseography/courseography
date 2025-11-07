{-|
Description: Filter Req Data module tests.

Module that contains the tests for the filtering of Req within the DynamicGraphs module.

-}

module RequirementTests.FilterReqTests
( test_filterReqs ) where

import Database.Requirement (Req (..))
import DynamicGraphs.GraphOptions (GraphOptions (..))
import DynamicGraphs.GraphGenerator (filterReq)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- Test constants
testGraphOptionsPopDept :: GraphOptions
testGraphOptionsPopDept = 
    GraphOptions { taken = ["CSC108H1"],
                departments = ["CSC", "STA"],
                excludedDepth = 0,
                maxDepth = (-1),
                courseNumPrefix = [],
                distribution = [],
                location = [],
                includeRaws = True,
                includeGrades = True
                }

testGraphOptionsEmptyDept :: GraphOptions
testGraphOptionsEmptyDept = 
    GraphOptions { taken = ["CSC108H1"],
                departments = [],
                excludedDepth = 0,
                maxDepth = (-1),
                courseNumPrefix = [],
                distribution = [],
                location = [],
                includeRaws = True,
                includeGrades = True
                }

testGraphOptionsEmptyAll :: GraphOptions
testGraphOptionsEmptyAll = 
    GraphOptions { taken = [],
                departments = [],
                excludedDepth = 0,
                maxDepth = (-1),
                courseNumPrefix = [],
                distribution = [],
                location = [],
                includeRaws = True,
                includeGrades = True
                }

reqCSC108 :: Req
reqCSC108 = J "CSC108H1" ""

reqCSC148 :: Req
reqCSC148 = J "CSC148H1" ""

reqCSC207 :: Req
reqCSC207 = ReqAnd [J "CSC207H1" "", reqCSC148, reqCSC108]

reqCSC209 :: Req
reqCSC209 = ReqAnd [J "CSC209H1" "", reqCSC207, J "CSC236H1" ""]

reqCSC369 :: Req
reqCSC369 = ReqAnd [J "CSC369H1" "", reqCSC209]

reqCSC207no108 :: Req
reqCSC207no108 = ReqAnd [J "CSC207H1" "", reqCSC148]

reqCSC209no108 :: Req
reqCSC209no108 = ReqAnd [J "CSC209H1" "", reqCSC207no108, J "CSC236H1" ""]

-- | List of (description, input requirements, input GraphOptions, expected filtered result)
filterReqTestCases :: [(String, Req, GraphOptions, Req)]
filterReqTestCases =
  [("Removes already taken courses",
  reqCSC207,
  testGraphOptionsPopDept,
  reqCSC207no108
  ),
  ("Does not filer out valid departments",
  ReqAnd [J "CSC148H1" "", J "STA257H1" ""],
  testGraphOptionsPopDept,
  ReqAnd [J "CSC148H1" "", J "STA257H1" ""]
  ),
  ("Filters out courses not in a valid department",
  ReqAnd [J "MAT137Y1" "", J "CSC207H1" ""],
  testGraphOptionsPopDept,
  J "CSC207H1" ""
  ),
  ("Handles ReqAnd",
  reqCSC207,
  testGraphOptionsPopDept,
  ReqAnd [J "CSC207H1" "", reqCSC148]
  ),
  ("Handles None input",
  None,
  testGraphOptionsPopDept, 
  None
  ),
  ("Returns None when both in an or should be removed",
  ReqOr [J "MAT137Y1" "", J "MAT237Y1" ""],
  testGraphOptionsPopDept,
  None  
  ),
  ("Removes OR when only one req remains",
  ReqOr [J "MAT137Y1" "", reqCSC207],
  testGraphOptionsPopDept,
  ReqAnd [J "CSC207H1" "", reqCSC148]
  ),
  ("Removes Nothing from dept filter if departments is empty in GraphOptions",
  ReqOr [J "MAT137Y1" "", J "MAT237Y1" ""],
  testGraphOptionsEmptyDept,
  ReqOr [J "MAT137Y1" "", J "MAT237Y1" ""]
  ),
  ("Removes nothing from a height of > 1 tree with no departments",
  reqCSC369,
  testGraphOptionsEmptyAll,
  reqCSC369
  ),
  ("Removes only CSC108H1 at the bottom of a height > 1 tree and empty department",
  reqCSC369,
  testGraphOptionsEmptyDept,
  ReqAnd [J "CSC369H1" "", reqCSC209no108]
  )
  ]

-- | Helper to run a single test case
runFilterReqTest :: String -> Req -> GraphOptions -> Req -> TestTree
runFilterReqTest description input options expected =
  testCase description $
    assertEqual ("Failed test: " ++ description)
      expected
      (filterReq options input)

-- | Generate all tests from the list above
runFilterReqTests :: [TestTree]
runFilterReqTests =
  [runFilterReqTest name input options expected | (name, input, options, expected) <- filterReqTestCases]

-- | Test suite for FilterReq
test_filterReqs :: TestTree
test_filterReqs = testGroup "FilterReq Tests" runFilterReqTests
