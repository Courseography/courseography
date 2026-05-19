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
    [ ("Invalid meeting (empty JSON), Nothing returned", "{}", Nothing)
    , ("Valid meeting with valid teachMethod", "{\"teachMethod\":\"LEC\"}", Just (Meeting "" "" "LEC" (-1) "" 0 0 0))
    , ("Valid meeting with all fields", "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"maxEnrolment\":100,\"currentEnrolment\":77,\"currentWaitlist\":0,\"instructors\":[{\"firstName\":\"Brinda\",\"lastName\":\"Venkataramani\"}]}", Just (Meeting "" "" "LEC0101" 100 "Brinda. Venkataramani" 77 0 0))
    , ("Valid meeting with no maxEnrolment, default cap returned", "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\"}", Just (Meeting "" "" "LEC0101" (-1) "" 0 0 0))
    , ("Valid meeting with no currentEnrolment, default enrol returned", "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"maxEnrolment\":100}", Just (Meeting "" "" "LEC0101" 100 "" 0 0 0))
    , ("Valid meeting with no currentWaitlist, default wait returned", "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"maxEnrolment\":100,\"currentEnrolment\":50}", Just (Meeting "" "" "LEC0101" 100 "" 50 0 0))
    , ("Valid meeting with multiple instructors", "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"instructors\":[{\"firstName\":\"A\",\"lastName\":\"B\"},{\"firstName\":\"C\",\"lastName\":\"D\"}]}", Just (Meeting "" "" "LEC0101" (-1) "A. B; C. D" 0 0 0))
    , ("Invalid meeting with no teachMethod, Nothing returned", "{\"sectionNumber\":\"0101\",\"maxEnrolment\":100}", Nothing)
    , ("Invalid meeting with unknown teachMethod, Nothing returned", "{\"teachMethod\":\"LAB\",\"sectionNumber\":\"0101\"}", Nothing)
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

-- | Test suite for Tables Module
test_tables :: TestTree
test_tables =
    testGroup "Parsing from JSON to Meeting tests" runMeetingFromJSONTests
