-- |
-- Description: Tables module tests.
--
-- Module that contains the tests for the functions in the Tables module.
module Database.TablesTests (
    test_tables,
) where

import Data.Aeson (Value, decode, decodeStrictText)
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Database.Persist.Sqlite (toSqlKey)
import Database.Tables (Meeting (..), MeetingId, Times (..), parseTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | List of test cases as (label, input JSON payload, expected output)
meetingFromJSONTestCases :: [(String, BL.ByteString, Maybe Meeting)]
meetingFromJSONTestCases =
    [ ("Invalid meeting (empty JSON), Nothing returned", "{}", Nothing)
    , ("Valid meeting with valid teachMethod", "{\"teachMethod\":\"LEC\"}", Just (Meeting "" "" "LEC" (-1) "" 0 0 0))
    ,
        ( "Valid meeting with all fields"
        , "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"maxEnrolment\":100,\"currentEnrolment\":77,\"currentWaitlist\":0,\"instructors\":[{\"firstName\":\"Brinda\",\"lastName\":\"Venkataramani\"}]}"
        , Just (Meeting "" "" "LEC0101" 100 "Brinda Venkataramani" 77 0 0)
        )
    ,
        ( "Valid meeting with no maxEnrolment, default cap returned"
        , "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\"}"
        , Just (Meeting "" "" "LEC0101" (-1) "" 0 0 0)
        )
    ,
        ( "Valid meeting with no currentEnrolment, default enrol returned"
        , "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"maxEnrolment\":100}"
        , Just (Meeting "" "" "LEC0101" 100 "" 0 0 0)
        )
    ,
        ( "Valid meeting with no currentWaitlist, default wait returned"
        , "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"maxEnrolment\":100,\"currentEnrolment\":50}"
        , Just (Meeting "" "" "LEC0101" 100 "" 50 0 0)
        )
    ,
        ( "Valid meeting with multiple instructors"
        , "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"instructors\":[{\"firstName\":\"A\",\"lastName\":\"B\"},{\"firstName\":\"C\",\"lastName\":\"D\"}]}"
        , Just (Meeting "" "" "LEC0101" (-1) "A B; C D" 0 0 0)
        )
    , ("Invalid meeting with no teachMethod, Nothing returned", "{\"sectionNumber\":\"0101\",\"maxEnrolment\":100}", Nothing)
    ,
        ( "Invalid meeting with unknown teachMethod, Nothing returned"
        , "{\"teachMethod\":\"LAB\",\"sectionNumber\":\"0101\"}"
        , Nothing
        )
    ]

-- | Run a test case (case, input, expected output) on the FromJSON instance of Meeting.
runMeetingFromJSONTest :: (String, BL.ByteString, Maybe Meeting) -> TestTree
runMeetingFromJSONTest (label, meetingJSON, expected) =
    testCase label $ do
        let actual = decode meetingJSON :: Maybe Meeting
        assertEqual ("Unexpected parsing result for " ++ label) expected actual

-- | Run all the meetingFromJSON test cases
runMeetingFromJSONTests :: [TestTree]
runMeetingFromJSONTests = map runMeetingFromJSONTest meetingFromJSONTestCases

dummyMeetingId :: MeetingId
dummyMeetingId = toSqlKey 1

-- | List of test cases as (label, input JSON string, expected output)
parseTimeTestCases :: [(String, T.Text, Maybe Times)]
parseTimeTestCases =
    [ ("Empty JSON string, Nothing returned", "", Nothing)
    ,
        ( "Valid JSON string"
        , "{ \"start\": { \"day\": 1, \"millisofday\": 36000000 }, \"end\": { \"millisofday\": 39600000 }, \"building\": { \"buildingCode\": \"BA\", \"buildingRoomNumber\": \"1130\" }, \"sessionCode\": \"20269\" }"
        , Just
            (Times (Just "20269") 0.0 10.0 11.0 dummyMeetingId (Just "BA"))
        )
    ,
        ( "Valid JSON string with no day, default time values returned"
        , "{ \"start\": { \"millisofday\": 43200000 }, \"end\": { \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" }, \"sessionCode\": \"20271\" }"
        , Just
            (Times (Just "20271") 5.0 25.0 25.0 dummyMeetingId (Just "MY"))
        )
    ,
        ( "Valid JSON string with no start millisofday, default time values returned"
        , "{ \"start\": { \"day\": 3 }, \"end\": { \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" }, \"sessionCode\": \"20271\" }"
        , Just
            (Times (Just "20271") 5.0 25.0 25.0 dummyMeetingId (Just "MY"))
        )
    ,
        ( "Valid JSON string with no end millisofday, default time values returned"
        , "{ \"start\": { \"day\": 3, \"millisofday\": 43200000 }, \"end\": { }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" }, \"sessionCode\": \"20271\" }"
        , Just
            (Times (Just "20271") 5.0 25.0 25.0 dummyMeetingId (Just "MY"))
        )
    ,
        ( "Invalid JSON string with no start value, Nothing returned"
        , "{ \"end\": { \"millisofday\": 54000000 }, \"building\": { \"buildingCode\": \"MP\", \"buildingRoomNumber\": \"202\" }, \"sessionCode\": \"20269\" }"
        , Nothing
        )
    ,
        ( "Invalid JSON string with no end value, Nothing returned"
        , "{ \"start\": { \"day\": 4, \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MP\", \"buildingRoomNumber\": \"202\" }, \"sessionCode\": \"20269\" }"
        , Nothing
        )
    ,
        ( "Invalid JSON string with no buildingCode value, Nothing returned"
        , "{ \"start\": { \"day\": 4, \"millisofday\": 50400000 }, \"end\": { \"millisofday\": 54000000 }, \"building\": { \"buildingRoomNumber\": \"202\" }, \"sessionCode\": \"20269\" }"
        , Nothing
        )
    ,
        ( "Invalid JSON string with no sessionCode value, Nothing returned"
        , "{ \"start\": { \"day\": 4, \"millisofday\": 50400000 }, \"end\": { \"millisofday\": 54000000 }, \"building\": { \"buildingCode\": \"MP\", \"buildingRoomNumber\": \"202\" } }"
        , Nothing
        )
    ]

-- | Run a test case (label, input JSON string, expected output) on the parseTime function.
runParseTimeTest :: (String, T.Text, Maybe Times) -> TestTree
runParseTimeTest (label, input, expected) =
    testCase label $ do
        let decodedValue = decodeStrictText input :: Maybe Value
            parsedFn = decodedValue >>= parseMaybe parseTime
            actualTimes = fmap ($ dummyMeetingId) parsedFn
        assertEqual ("Unexpected parsing result for " ++ label) expected actualTimes

-- | Run all the parseTime test cases
runParseTimeTests :: [TestTree]
runParseTimeTests = map runParseTimeTest parseTimeTestCases

-- | Test suite for Tables Module
test_tables :: TestTree
test_tables =
    testGroup "Tables tests" $ runMeetingFromJSONTests ++ runParseTimeTests
