{-|
Description: Tables module tests.

Module that contains the tests for the functions in the Tables module.

-}

module Database.TablesTests
( test_tables
) where

import Data.Aeson (decode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Database.Tables (Time' (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | List of test cases as (label, input JSON string, expected output)
time'FromJSONTestCases :: [(String, T.Text, Maybe Time')]
time'FromJSONTestCases =
    [ ("Empty JSON string", "", Nothing)
    , ("Valid JSON string", "{ \"start\": { \"day\": 1, \"millisofday\": 36000000 }, \"end\": { \"millisofday\": 39600000 }, \"building\": { \"buildingCode\": \"BA\", \"buildingRoomNumber\": \"1130\" }, \"assignedRoom2\": \"MP102\" }", Just (Time' {weekDay' = 0.0, startHour' = 10.0, endHour' = 11.0, firstLocation' = Just "BA1130", secondLocation' = Just "MP102"}))
    , ("Valid JSON string with no second location", "{ \"start\": { \"day\": 2, \"millisofday\": 39600000 }, \"end\": { \"millisofday\": 43200000 }, \"building\": { \"buildingCode\": \"MP\", \"buildingRoomNumber\": \"203\" } }", Just (Time' {weekDay' = 1.0, startHour' = 11.0, endHour' = 12.0, firstLocation' = Just "MP203", secondLocation' = Nothing}))
    , ("Invalid JSON string (no day value)", "{ \"start\": { \"millisofday\": 43200000 }, \"end\": { \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" } }", Just (Time' {weekDay' = 5.0, startHour' = 25.0, endHour' = 25.0, firstLocation' = Just "MY150", secondLocation' = Nothing}))
    , ("Invalid JSON string (no start millisofday value)", "{ \"start\": { \"day\": 3 }, \"end\": { \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" } }", Just (Time' {weekDay' = 5.0, startHour' = 25.0, endHour' = 25.0, firstLocation' = Just "MY150", secondLocation' = Nothing}))
    , ("Invalid JSON string (no end millisofday value)", "{ \"start\": { \"day\": 3, \"millisofday\": 43200000 }, \"end\": { }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" } }", Just (Time' {weekDay' = 5.0, startHour' = 25.0, endHour' = 25.0, firstLocation' = Just "MY150", secondLocation' = Nothing}))
    , ("Invalid JSON string (no buildingCode value)", "{ \"day\": 4, \"start\": { \"millisofday\": 50400000 }, \"end\": { \"millisofday\": 54000000 }, \"building\": { \"buildingRoomNumber\": \"202\" } }", Nothing)
    , ("Invalid JSON string (no buildingRoomNumber value)", "{ \"day\": 4, \"start\": { \"millisofday\": 50400000 }, \"end\": { \"millisofday\": 54000000 }, \"building\": { \"buildingCode\": \"MP\" } }", Nothing)
    ]

-- | Run a test case (label, input JSON string, expected output) on the FromJSON instance of Time'.
runTime'FromJSONTest :: (String, T.Text, Maybe Time') -> TestTree
runTime'FromJSONTest (label, input, expected) =
    testCase label $ do
        let decoded = decode (BL.fromStrict (TE.encodeUtf8 input)) :: Maybe Time'
        assertEqual ("Unexpected response body for " ++ label) (show expected) (show decoded)

-- | Run all the time'FromJSON test cases
runTime'FromJSONTests :: [TestTree]
runTime'FromJSONTests = map runTime'FromJSONTest time'FromJSONTestCases

-- | Test suite for Tables Module
test_tables :: TestTree
test_tables =
    testGroup "Tables tests" runTime'FromJSONTests
