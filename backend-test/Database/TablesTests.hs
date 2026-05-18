{-|
Description: Tables module tests.

Module that contains the tests for the functions in the Tables module.

-}

module Database.TablesTests
( test_tables
) where

import Data.Aeson (decodeStrictText)
import qualified Data.Text as T
import Database.Tables (Time' (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | List of test cases as (label, input JSON string, expected output)
time'FromJSONTestCases :: [(String, T.Text, Maybe Time')]
time'FromJSONTestCases =
    [ ("Empty JSON string, Nothing returned", "", Nothing)
    , ("Valid JSON string", "{ \"start\": { \"day\": 1, \"millisofday\": 36000000 }, \"end\": { \"millisofday\": 39600000 }, \"building\": { \"buildingCode\": \"BA\", \"buildingRoomNumber\": \"1130\" }, \"assignedRoom2\": \"MP102\" }", Just (Time' {weekDay' = 0.0, startHour' = 10.0, endHour' = 11.0, firstLocation' = Just "BA1130", secondLocation' = Just "MP102"}))
    , ("Valid JSON string with no assignedRoom2", "{ \"start\": { \"day\": 2, \"millisofday\": 39600000 }, \"end\": { \"millisofday\": 43200000 }, \"building\": { \"buildingCode\": \"MP\", \"buildingRoomNumber\": \"203\" } }", Just (Time' {weekDay' = 1.0, startHour' = 11.0, endHour' = 12.0, firstLocation' = Just "MP203", secondLocation' = Nothing}))
    , ("Valid JSON string with no day, default time values returned", "{ \"start\": { \"millisofday\": 43200000 }, \"end\": { \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" } }", Just (Time' {weekDay' = 5.0, startHour' = 25.0, endHour' = 25.0, firstLocation' = Just "MY150", secondLocation' = Nothing}))
    , ("Valid JSON string with no start millisofday, default time values returned", "{ \"start\": { \"day\": 3 }, \"end\": { \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" } }", Just (Time' {weekDay' = 5.0, startHour' = 25.0, endHour' = 25.0, firstLocation' = Just "MY150", secondLocation' = Nothing}))
    , ("Valid JSON string with no end millisofday, default time values returned", "{ \"start\": { \"day\": 3, \"millisofday\": 43200000 }, \"end\": { }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" } }", Just (Time' {weekDay' = 5.0, startHour' = 25.0, endHour' = 25.0, firstLocation' = Just "MY150", secondLocation' = Nothing}))
    , ("Invalid JSON string with no start value, Nothing returned", "{ \"end\": { \"millisofday\": 54000000 }, \"building\": { \"buildingCode\": \"MP\", \"buildingRoomNumber\": \"202\" } }", Nothing)
    , ("Invalid JSON string with no end value, Nothing returned", "{ \"start\": { \"day\": 4, \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MP\", \"buildingRoomNumber\": \"202\" } }", Nothing)
    , ("Invalid JSON string with no buildingCode value, Nothing returned", "{ \"start\": { \"day\": 4, \"millisofday\": 50400000 }, \"end\": { \"millisofday\": 54000000 }, \"building\": { \"buildingRoomNumber\": \"202\" } }", Nothing)
    , ("Invalid JSON string with no buildingRoomNumber value, Nothing returned", "{ \"start\": { \"day\": 4, \"millisofday\": 50400000 }, \"end\": { \"millisofday\": 54000000 }, \"building\": { \"buildingCode\": \"MP\" } }", Nothing)
    ]

-- | Run a test case (label, input JSON string, expected output) on the FromJSON instance of Time'.
runTime'FromJSONTest :: (String, T.Text, Maybe Time') -> TestTree
runTime'FromJSONTest (label, input, expected) =
    testCase label $ do
        let decoded = decodeStrictText input :: Maybe Time'
        assertEqual ("Unexpected parsing result for " ++ label) expected decoded

-- | Run all the time'FromJSON test cases
runTime'FromJSONTests :: [TestTree]
runTime'FromJSONTests = map runTime'FromJSONTest time'FromJSONTestCases

-- | Test suite for Tables Module
test_tables :: TestTree
test_tables =
    testGroup "Tables tests" runTime'FromJSONTests
