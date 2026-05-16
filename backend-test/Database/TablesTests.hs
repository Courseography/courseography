{- HLINT ignore "Use forM_" -}
{-|
Description: Tables module tests.

Module that contains the tests for the functions in the Tables module.

-}

module Database.TablesTests
( test_tableQueries
) where

import Config (runDb)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.Persist.Sqlite (insert_)
import Database.Tables (Meeting (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, withDatabase)

-- | List of test cases as (label, input JSON payload, expected output)
fromJSONToMeetingTestCases :: [(String, BL.ByteString, Maybe Meeting)]
fromJSONToMeetingTestCases =
    [ ("Invalid meeting returns Nothing", "{}", Nothing)
    , ("Valid meeting", "{\"teachMethod\":\"LEC\"}", Just (Meeting "" "" "LEC" (-1) "" 0 0 0))
    , ("Valid meeting with all fields", "{\"teachMethod\":\"LEC\",\"sectionNumber\":\"0101\",\"maxEnrolment\":100,\"currentEnrolment\":77,\"currentWaitlist\":0,\"instructors\":[{\"firstName\":\"Brinda\",\"lastName\":\"Venkataramani\"}]}", Just (Meeting "" "" "LEC0101" 100 "Brinda. Venkataramani" 77 0 0))
    ]

-- | Run a test case (case, input, expected output) on the FromJSON instance of Meeting.
runFromJSONToMeetingTest :: (String, BL.ByteString, Maybe Meeting) -> TestTree
runFromJSONToMeetingTest (label, meetingJSON, expected) =
    testCase label $ do
        let actual = decode meetingJSON :: Maybe Meeting

        runDb $ do
            clearDatabase
            case actual of
                Just meeting -> insert_ meeting
                Nothing -> return ()

        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the test cases on the FromJSON instance of Meeting
runFromJSONToMeetingTests :: [TestTree]
runFromJSONToMeetingTests = map runFromJSONToMeetingTest fromJSONToMeetingTestCases

-- | Test suite for Tables Module
test_tableQueries :: TestTree
test_tableQueries =
    withDatabase "Parsing from JSON to Meeting tests" runFromJSONToMeetingTests
