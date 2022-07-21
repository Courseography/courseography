{-|
Description: Test modifier string formatting using HUnit Testing Framework.

Module containing test cases for Modifier string formatters.

-}

module RequirementTests.ModifierTests
( modifierTestSuite ) where

import Database.Requirement
import DynamicGraphs.GraphNodeUtils (concatModOr, stringifyModAnd)
import Test.HUnit (Test (..), assertEqual)

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Eq a, Show a) => (a -> String) -> String -> [(a, String)] -> Test
createTest function label input = TestLabel label $ TestList $ map (\(x, y) ->
                                TestCase $ assertEqual ("for (" ++ y ++ ")")
                                y (function x)) input

-- Global FCEs value so the expected output has the same FCEs as the partial function in createTest
globalFces :: Float
globalFces = 1.0

concatModOrInputs :: [([Modifier], String)]
concatModOrInputs = [
      ([Department "CSC", Department "BCB"], "CSC/BCB")
    , ([Department "CSC", Department "BCB", Department "Statistics"], "CSC/BCB/Statistics")
    , ([Level "300", Level "400"], "300/400")
    , ([Requirement (Raw "lorem"), Requirement (Raw "ipsum")], "lorem or ipsum")
    ]

simpleModAndInputs :: [([Modifier], String)]
simpleModAndInputs = [
      ([Department "CSC", Level "300"], show globalFces ++ " CSC FCEs at the 300 level")
    , ([Department "CSC", Requirement (Raw "some raw text")], show globalFces ++ " CSC FCEs from some raw text")
    , ([Level "300+", Requirement (Raw "some raw text")], show globalFces ++ " FCEs at the 300+ level from some raw text")
    , ([Department "CSC", Level "300+", Requirement (Raw "some raw text")], show globalFces ++ " CSC FCEs at the 300+ level from some raw text")
    , ([ModOr [Level "300", Level "400"], Department "CSC"], show globalFces ++ " CSC FCEs at the 300/400 level")
    ]

modandModOrInputs :: [([Modifier], String)]
modandModOrInputs = [
      ([ModOr [Level "300", Level "400"], Department "CSC"], show globalFces ++ " CSC FCEs at the 300/400 level")
    , ([Level "300+", ModOr [Department "CSC", Department "BCB", Department "Statistics"]], show globalFces ++ " CSC/BCB/Statistics FCEs at the 300+ level")
    , ([ModOr [Level "300", Level "400"], ModOr [Department "CSC", Department "BCB"]], show globalFces ++ " CSC/BCB FCEs at the 300/400 level")
    , ([ModOr [Level "300", Level "400"], ModOr [Department "CSC", Department "BCB"], Requirement (Raw "some raw text")], show globalFces ++ " CSC/BCB FCEs at the 300/400 level from some raw text")
    ]

concatModOrTests :: Test
concatModOrTests = createTest concatModOr "joining ModOr with a delimiter" concatModOrInputs

simpleModAndTests :: Test
simpleModAndTests = createTest (stringifyModAnd globalFces) "ModAnd not containing ModOrs" simpleModAndInputs

modandModOrTests :: Test
modandModOrTests = createTest (stringifyModAnd globalFces) "ModAnd containing ModOrs" modandModOrInputs

-- functions for running tests in REPL
modifierTestSuite :: Test
modifierTestSuite = TestLabel "ReqParser tests" $ TestList [concatModOrTests, simpleModAndTests, modandModOrTests]
