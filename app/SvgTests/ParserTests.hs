module SvgTests.ParserTests where

import Svg.Parser
import Test.HUnit

{- So far, this module serves as an example of the style of testing
   HUnit is capable of executing. -}

-- |Test points.
testPts = [(i,j) | i <- [0..10], j <- [0..10]]

-- |Test block for addTuples.
test_addTuples = test ["addTuples test 1" ~: "addTuples (0,0) (0,0)"
                                          ~: (0,0)
                                          ~=? addTuples (0,0) (0,0),
                       "addTuples test 2" ~: "addTuples (1,1), (1,1)"
                                          ~: (2,2)
                                          ~=? addTuples (1,1) (1,1),
                       "addTuples test 3" ~: "addTuples (testPts !! 0) (10,10)"
                                          ~: (10,10)
                                          ~=? addTuples (testPts !! 0) (10,10),
                       "addTuples test 4" ~: "addTuples (-10,-10) (testPts !! 10)"
                                          ~: (-10, 0)
                                          ~=? addTuples (-10,-10) (testPts !! 10)]

