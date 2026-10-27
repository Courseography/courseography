-- |
-- Description: Meeting module tests.
--
-- Module that contains the tests for the functions in the Meeting module.
module Models.MeetingTests (
    test_meeting,
) where

import Data.Aeson (decodeStrictText)
import qualified Data.Text as T
import Database.Persist.Sqlite (toSqlKey)
import Database.Tables (Meeting (..), MeetingId, Time (..))
import Models.Meeting (MeetingParsedData (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | List of test cases as (label, input JSON payload, expected output)
parseMeetingTestCases :: [(String, T.Text, Maybe Meeting)]
parseMeetingTestCases =
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
    ,
        ( "Valid meeting with malformed instructors, empty instructor returned"
        , "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"instructors\":[\"abc\"]}"
        , Just (Meeting "" "" "LEC0101" (-1) "" 0 0 0)
        )
    ]

-- | Run a test case (label, input JSON payload, expected output) on the parsing of a meeting.
runParseMeetingTest :: (String, T.Text, Maybe Meeting) -> TestTree
runParseMeetingTest (label, meetingJSON, expected) =
    testCase label $ do
        let actual = meetInfo <$> (decodeStrictText meetingJSON :: Maybe MeetingParsedData)
        assertEqual ("Unexpected parsing result for " ++ label) expected actual

-- | Run all the parseMeeting test cases
runParseMeetingTests :: [TestTree]
runParseMeetingTests = map runParseMeetingTest parseMeetingTestCases

dummyMeetingId :: MeetingId
dummyMeetingId = toSqlKey 1

-- | List of test cases as (label, input JSON string, expected output)
parseTimeTestCases :: [(String, T.Text, Maybe Time)]
parseTimeTestCases =
    [ ("Empty JSON string, Nothing returned", "", Nothing)
    ,
        ( "Valid JSON string"
        , "{ \"start\": { \"day\": 1, \"millisofday\": 36000000 }, \"end\": { \"millisofday\": 39600000 }, \"building\": { \"buildingCode\": \"BA\", \"buildingRoomNumber\": \"1130\" }, \"sessionCode\": \"20269\" }"
        , Just
            (Time (Just "20269") 0.0 10.0 11.0 dummyMeetingId (Just "BA"))
        )
    ,
        ( "Valid JSON string with no day, default time values returned"
        , "{ \"start\": { \"millisofday\": 43200000 }, \"end\": { \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" }, \"sessionCode\": \"20271\" }"
        , Just
            (Time (Just "20271") 5.0 25.0 25.0 dummyMeetingId (Just "MY"))
        )
    ,
        ( "Valid JSON string with no start millisofday, default time values returned"
        , "{ \"start\": { \"day\": 3 }, \"end\": { \"millisofday\": 50400000 }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" }, \"sessionCode\": \"20271\" }"
        , Just
            (Time (Just "20271") 5.0 25.0 25.0 dummyMeetingId (Just "MY"))
        )
    ,
        ( "Valid JSON string with no end millisofday, default time values returned"
        , "{ \"start\": { \"day\": 3, \"millisofday\": 43200000 }, \"end\": { }, \"building\": { \"buildingCode\": \"MY\", \"buildingRoomNumber\": \"150\" }, \"sessionCode\": \"20271\" }"
        , Just
            (Time (Just "20271") 5.0 25.0 25.0 dummyMeetingId (Just "MY"))
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

-- | Run a test case (label, input JSON string, expected output) on the parsing of a meeting time.
-- The time is wrapped in a minimal valid meeting so it is parsed by the FromJSON instance of MeetingParsedData.
runParseTimeTest :: (String, T.Text, Maybe Time) -> TestTree
runParseTimeTest (label, input, expected) =
    testCase label $ do
        let meetingJSON = T.concat ["{\"teachMethod\":\"LEC\",\"meetingTimes\":[", input, "]}"]
            parsedMeeting = decodeStrictText meetingJSON :: Maybe MeetingParsedData
            actualTimes =
                parsedMeeting >>= \m -> case timeInfo m of
                    [timeFn] -> Just (timeFn dummyMeetingId)
                    _ -> Nothing
        assertEqual ("Unexpected parsing result for " ++ label) expected actualTimes

-- | Run all the parseTime test cases
runParseTimeTests :: [TestTree]
runParseTimeTests = map runParseTimeTest parseTimeTestCases

-- | Test suite for Meeting Module
test_meeting :: TestTree
test_meeting =
    testGroup "Meeting tests" $ runParseMeetingTests ++ runParseTimeTests
