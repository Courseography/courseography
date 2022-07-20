{-|
Description: Test modifier string formatting using HUnit Testing Framework.

Module containing test cases for Modifier string formatters.

-}

module RequirementTests.ModifierTests
( modifierTestSuite ) where

import Database.Requirement
import DynamicGraphs.GraphNodeUtils (concatModor, stringifyModand)
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
      ([Department "CSC", Department "BCB"], "CSC/BCB")
    , ([Department "CSC", Department "BCB", Department "Statistics"], "CSC/BCB/Statistics")
    , ([Level "300", Level "400"], "300/400")
    , ([Requirement (Raw "lorem"), Requirement (Raw "ipsum")], "lorem or ipsum")
    ]

simpleModandInputs :: [([Modifier], String)]
simpleModandInputs = [
      ([Department "CSC", Level "300"], show globalFCEs ++ " CSC FCEs at the 300 level")
    , ([Department "CSC", Requirement (Raw "some raw text")], show globalFCEs ++ " CSC FCEs from some raw text")
    , ([Level "300+", Requirement (Raw "some raw text")], show globalFCEs ++ " FCEs at the 300+ level from some raw text")
    , ([Department "CSC", Level "300+", Requirement (Raw "some raw text")], show globalFCEs ++ " CSC FCEs at the 300+ level from some raw text")
    , ([ModOr [Level "300", Level "400"], Department "CSC"], show globalFCEs ++ " CSC FCEs at the 300/400 level")
    ]

modandModorInputs :: [([Modifier], String)]
modandModorInputs = [
      ([ModOr [Level "300", Level "400"], Department "CSC"], show globalFCEs ++ " CSC FCEs at the 300/400 level")
    , ([Level "300+", ModOr [Department "CSC", Department "BCB", Department "Statistics"]], show globalFCEs ++ " CSC/BCB/Statistics FCEs at the 300+ level")
    , ([ModOr [Level "300", Level "400"], ModOr [Department "CSC", Department "BCB"]], show globalFCEs ++ " CSC/BCB FCEs at the 300/400 level")
    , ([ModOr [Level "300", Level "400"], ModOr [Department "CSC", Department "BCB"], Requirement (Raw "some raw text")], show globalFCEs ++ " CSC/BCB FCEs at the 300/400 level from some raw text")
    ]

concatModorTests :: Test
concatModorTests = createTest concatModor "joining ModOr with a delimiter" concatModorInputs

simpleModandTests :: Test
simpleModandTests = createTest (stringifyModand globalFCEs) "ModAnd not containing ModOrs" simpleModandInputs

modandModorTests :: Test
modandModorTests = createTest (stringifyModand globalFCEs) "ModAnd containing ModOrs" modandModorInputs

-- functions for running tests in REPL
modifierTestSuite :: Test
modifierTestSuite = TestLabel "ReqParser tests" $ TestList [concatModorTests, simpleModandTests, modandModorTests]
