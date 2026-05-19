{- HLINT ignore "Use forM_" -}
{-|
Description: Tables module tests.

Module that contains the tests for the functions in the Tables module.

-}

module Database.TablesTests
( test_tables
) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.Tables (Meeting (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | List of test cases as (label, input JSON payload, expected output)
meetingFromJSONTestCases :: [(String, BL.ByteString, Maybe Meeting)]
meetingFromJSONTestCases =
    [ ("Invalid meeting returns Nothing", "{}", Nothing)
    , ("Valid meeting", "{\"teachMethod\":\"LEC\"}", Just (Meeting "" "" "LEC" (-1) "" 0 0 0))
    , ("Valid meeting with all fields", "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"maxEnrolment\":100,\"currentEnrolment\":77,\"currentWaitlist\":0,\"instructors\":[{\"firstName\":\"Brinda\",\"lastName\":\"Venkataramani\"}]}", Just (Meeting "" "" "LEC0101" 100 "Brinda. Venkataramani" 77 0 0))
    ]

-- | Run a test case (case, input, expected output) on the FromJSON instance of Meeting.
runMeetingFromJSONTest :: (String, BL.ByteString, Maybe Meeting) -> TestTree
runMeetingFromJSONTest (label, meetingJSON, expected) =
    testCase label $ do
        let actual = decode meetingJSON :: Maybe Meeting
        assertEqual ("Unexpected parsing result for " ++ label) expected actual

-- | Run all the test cases on the FromJSON instance of Meeting
runMeetingFromJSONTests :: [TestTree]
runMeetingFromJSONTests = map runMeetingFromJSONTest meetingFromJSONTestCases

-- | Test suite for Tables Module
test_tables :: TestTree
test_tables =
    testGroup "Parsing from JSON to Meeting tests" runMeetingFromJSONTests
