{-|
Description: Filter Req Data module tests.

Module that contains the tests for the filtering of Req within the DynamicGraphs module.

-}

module RequirementTests.FilterReqTests
( test_filterReqs ) where

import Database.Requirement (Req (..))
import DynamicGraphs.GraphGenerator (filterReq)
import DynamicGraphs.GraphOptions (GraphOptions (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- Test GraphOptions
graphOptionsSimple :: GraphOptions
graphOptionsSimple =
  GraphOptions { taken = [],
                departments = [],
                excludedDepth = 0,
                maxDepth = -1,
                courseNumPrefix = [],
                distribution = [],
                location = [],
                includeRaws = True,
                includeGrades = True
                }

graphOptionsWithDepartmentsAndTaken :: GraphOptions
graphOptionsWithDepartmentsAndTaken =
  GraphOptions { taken = ["CSC108H1"],
                departments = ["CSC", "STA"],
                excludedDepth = 0,
                maxDepth = -1,
                courseNumPrefix = [],
                distribution = [],
                location = [],
                includeRaws = True,
                includeGrades = True
                }

graphOptionsWithTaken :: GraphOptions
graphOptionsWithTaken =
   GraphOptions { taken = ["CSC108H1"],
                departments = [],
                excludedDepth = 0,
                maxDepth = -1,
                courseNumPrefix = [],
                distribution = [],
                location = [],
                includeRaws = True,
                includeGrades = True
                }

-- | List of (description, input requirements, input GraphOptions, expected filtered result)
filterReqTestCases :: [(String, Req, GraphOptions, Req)]
filterReqTestCases =
  [("Removes already taken courses",
  ReqAnd [J "CSC207H1" "", J "CSC148H1" "", J "CSC108H1" ""],
  graphOptionsWithDepartmentsAndTaken,
  ReqAnd [J "CSC207H1" "", J "CSC148H1" ""]
  ),
  ("Does not filer out valid departments",
  ReqAnd [J "CSC148H1" "", J "STA257H1" ""],
  graphOptionsWithDepartmentsAndTaken,
  ReqAnd [J "CSC148H1" "", J "STA257H1" ""]
  ),
  ("Filters out courses not in a valid department",
  ReqAnd [J "MAT137Y1" "", J "CSC207H1" ""],
  graphOptionsWithDepartmentsAndTaken,
  J "CSC207H1" ""
  ),
  ("Handles ReqAnd",
  ReqAnd [J "CSC207H1" "", J "CSC148H1" "", J "CSC108H1" ""],
  graphOptionsWithDepartmentsAndTaken,
  ReqAnd [J "CSC207H1" "", J "CSC148H1" ""]
  ),
  ("Handles None input",
  None,
  graphOptionsWithDepartmentsAndTaken,
  None
  ),
  ("Returns None when both in an or should be removed",
  ReqOr [J "MAT137Y1" "", J "MAT237Y1" ""],
  graphOptionsWithDepartmentsAndTaken,
  None
  ),
  ("Removes OR when only one req remains",
  ReqOr [J "MAT137Y1" "", ReqAnd [J "CSC207H1" "", J "CSC148H1" "", J "CSC108H1" ""]],
  graphOptionsWithDepartmentsAndTaken,
  ReqAnd [J "CSC207H1" "", J "CSC148H1" ""]
  ),
  ("Removes Nothing from dept filter if departments is empty in GraphOptions",
  ReqOr [J "MAT137Y1" "", J "MAT237Y1" ""],
  graphOptionsWithTaken,
  ReqOr [J "MAT137Y1" "", J "MAT237Y1" ""]
  ),
  ("Removes nothing from a height of > 1 tree with no departments",
  ReqAnd [J "CSC369H1" "", ReqAnd [J "CSC209H1" "", ReqAnd [J "CSC207H1" "", J "CSC148H1" "", J "CSC108H1" ""], J "CSC236H1" ""]],
  graphOptionsSimple,
  ReqAnd [J "CSC369H1" "", ReqAnd [J "CSC209H1" "", ReqAnd [J "CSC207H1" "", J "CSC148H1" "", J "CSC108H1" ""], J "CSC236H1" ""]]
  ),
  ("Removes only CSC108H1 at the bottom of a height > 1 tree and empty department",
  ReqAnd [J "CSC369H1" "", ReqAnd [J "CSC209H1" "", ReqAnd [J "CSC207H1" "", J "CSC148H1" "", J "CSC108H1" ""], J "CSC236H1" ""]],
  graphOptionsWithTaken,
  ReqAnd [J "CSC369H1" "", ReqAnd [J "CSC209H1" "", ReqAnd [J "CSC207H1" "", J "CSC148H1" ""], J "CSC236H1" ""]]
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
