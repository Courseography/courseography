{-|
Description: Test modifier string formatting using HUnit Testing Framework.

Module containing test cases for Modifier string formatters.

-}

module RequirementTests.ModifierTests
( modifierTestSuite ) where

import Database.Requirement
import DynamicGraphs.GraphGenerator (concatModor, stringifyModand)
import Test.HUnit (Test (..), assertEqual)

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Eq a, Show a) => (a -> String) -> String -> [(a, String)] -> Test
createTest function label input = TestLabel label $ TestList $ map (\(x, y) ->
                                TestCase $ assertEqual ("for (" ++ y ++ ")")
                                y (function x)) input

-- Global FCEs value so the expected output has the same FCEs as the partial function in createTest
globalFCEs :: Float
globalFCEs = 1.0

concatModorInputs :: [([Modifier], String)]
concatModorInputs = [
      ([DEPARTMENT "CSC", DEPARTMENT "BCB"], "CSC/BCB")
    , ([DEPARTMENT "CSC", DEPARTMENT "BCB", DEPARTMENT "Statistics"], "CSC/BCB/Statistics")
    , ([LEVEL "300", LEVEL "400"], "300/400")
    , ([REQUIREMENT (RAW "lorem"), REQUIREMENT (RAW "ipsum")], "lorem or ipsum")
    ]

simpleModandInputs :: [([Modifier], String)]
simpleModandInputs = [
      ([DEPARTMENT "CSC", LEVEL "300"], show globalFCEs ++ " CSC FCEs at the 300 level")
    , ([DEPARTMENT "CSC", REQUIREMENT (RAW "some raw text")], show globalFCEs ++ " CSC FCEs from some raw text")
    , ([LEVEL "300+", REQUIREMENT (RAW "some raw text")], show globalFCEs ++ " FCEs at the 300+ level from some raw text")
    , ([DEPARTMENT "CSC", LEVEL "300+", REQUIREMENT (RAW "some raw text")], show globalFCEs ++ " CSC FCEs at the 300+ level from some raw text")
    , ([MODOR [LEVEL "300", LEVEL "400"], DEPARTMENT "CSC"], show globalFCEs ++ " CSC FCEs at the 300/400 level")
    ]

modandModorInputs :: [([Modifier], String)]
modandModorInputs = [
      ([MODOR [LEVEL "300", LEVEL "400"], DEPARTMENT "CSC"], show globalFCEs ++ " CSC FCEs at the 300/400 level")
    , ([LEVEL "300+", MODOR [DEPARTMENT "CSC", DEPARTMENT "BCB", DEPARTMENT "Statistics"]], show globalFCEs ++ " CSC/BCB/Statistics FCEs at the 300+ level")
    , ([MODOR [LEVEL "300", LEVEL "400"], MODOR [DEPARTMENT "CSC", DEPARTMENT "BCB"]], show globalFCEs ++ " CSC/BCB FCEs at the 300/400 level")
    , ([MODOR [LEVEL "300", LEVEL "400"], MODOR [DEPARTMENT "CSC", DEPARTMENT "BCB"], REQUIREMENT (RAW "some raw text")], show globalFCEs ++ " CSC/BCB FCEs at the 300/400 level from some raw text")
    ]

concatModorTests :: Test
concatModorTests = createTest concatModor "joining MODOR with a delimiter" concatModorInputs

simpleModandTests :: Test
simpleModandTests = createTest (stringifyModand globalFCEs) "MODAND not containing MODORs" simpleModandInputs

modandModorTests :: Test
modandModorTests = createTest (stringifyModand globalFCEs) "MODAND containing MODORs" modandModorInputs

-- functions for running tests in REPL
modifierTestSuite :: Test
modifierTestSuite = TestLabel "ReqParser tests" $ TestList [concatModorTests, simpleModandTests, modandModorTests]
