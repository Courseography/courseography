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
import Database.Tables (MeetingId, Times (..))
import Models.Meeting (MeetingParsedData (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

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

-- | Run a test case (label, input JSON string, expected output) on the parsing of a meeting time.
-- The time is wrapped in a minimal valid meeting so it is parsed by the FromJSON instance of MeetingParsedData.
runParseTimeTest :: (String, T.Text, Maybe Times) -> TestTree
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
    testGroup "Meeting tests" runParseTimeTests
