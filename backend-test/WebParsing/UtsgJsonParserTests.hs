-- |
-- Description: UtsgJsonParser module tests.
--
-- Module that contains the tests for the functions in the UtsgJsonParser module.
--
-- The expected meetings and times are rows taken from @db/database.sqlite3@, and each
-- response body is the timetable API payload those rows were parsed from. Note that the
-- API sends a course code with its credit suffix (@CSC207H1@), which the parser drops.
module WebParsing.UtsgJsonParserTests (
    test_utsgJsonParser,
) where

import Config (runDb)
import qualified Data.ByteString.Lazy.Char8 as BL
import Database.Persist.Sqlite (Entity, Filter, SelectOpt (Asc), entityKey, entityVal, selectList, (==.))
import Database.Tables (EntityField (..), Meeting (..), MeetingId, Time' (..), Times (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, withDatabase)
import WebParsing.UtsgJsonParser (insertCourses)

-- | All the meetings currently in the database, ordered by code and then section
getMeetings :: IO [Entity Meeting]
getMeetings = runDb $ selectList ([] :: [Filter Meeting]) [Asc MeetingCode, Asc MeetingSection]

-- | The times of the given meeting, in the order they were inserted. They are returned as
--   Time' values (the times of a meeting before it has been inserted) so that a test case
--   does not have to know the generated MeetingId to state what it expects.
getTimesOf :: MeetingId -> IO [Time']
getTimesOf meetingKey = do
    times <- runDb $ selectList [TimesMeeting ==. meetingKey] [Asc TimesId]
    return $ map (toTime' . entityVal) times
  where
    toTime' :: Times -> Time'
    toTime' t = Time' (timesSession t) (timesWeekDay t) (timesStartHour t) (timesEndHour t) (timesLocation t)

-- | List of test cases as (label, input response body, expected meetings each paired with their expected times)
insertCoursesTestCases :: [(String, BL.ByteString, [(Meeting, [Time'])])]
insertCoursesTestCases =
    [
        ( "Single course with a single lecture section"
        , "{\"payload\": {\"pageableCourse\": {\"courses\": [\
          \{\"code\": \"CSC207H1\", \"sectionCode\": \"F\", \"sections\": [\
          \{\"teachMethod\": \"LEC\", \"sectionNumber\": \"0201\", \"maxEnrolment\": 150, \"currentEnrolment\": 150,\
          \ \"currentWaitlist\": 2, \"instructors\": [{\"firstName\": \"Lindsey\", \"lastName\": \"Shorser\"}],\
          \ \"meetingTimes\": [{\"start\": {\"day\": 3, \"millisofday\": 54000000},\
          \ \"end\": {\"millisofday\": 61200000}, \"building\": {\"buildingCode\": \"MP\"},\
          \ \"sessionCode\": \"20269\"}]}]}]}}}"
        , [(Meeting "CSC207" "F" "LEC0201" 150 "Lindsey Shorser" 150 2 0, [Time' (Just "20269") 2.0 15.0 17.0 (Just "MP")])]
        )
    ,
        ( "Year-long section with several instructors, taught in both sessions"
        , "{\"payload\": {\"pageableCourse\": {\"courses\": [\
          \{\"code\": \"ANT100Y1\", \"sectionCode\": \"Y\", \"sections\": [\
          \{\"teachMethod\": \"LEC\", \"sectionNumber\": \"0101\", \"maxEnrolment\": 488, \"currentEnrolment\": 428,\
          \ \"instructors\": [{\"firstName\": \"Katherine\", \"lastName\": \"Patton\"},\
          \ {\"firstName\": \"Shawn\", \"lastName\": \"Lehman\"}, {\"firstName\": \"Ivan\", \"lastName\": \"Kalmar\"}],\
          \ \"meetingTimes\": [\
          \{\"start\": {\"day\": 4, \"millisofday\": 54000000}, \"end\": {\"millisofday\": 61200000},\
          \ \"building\": {\"buildingCode\": \"OI\"}, \"sessionCode\": \"20269\"},\
          \{\"start\": {\"day\": 4, \"millisofday\": 54000000}, \"end\": {\"millisofday\": 61200000},\
          \ \"building\": {\"buildingCode\": \"OI\"}, \"sessionCode\": \"20271\"}]}]}]}}}"
        ,
            [
                ( Meeting "ANT100" "Y" "LEC0101" 488 "Katherine Patton; Shawn Lehman; Ivan Kalmar" 428 0 0
                ,
                    [ Time' (Just "20269") 3.0 15.0 17.0 (Just "OI")
                    , Time' (Just "20271") 3.0 15.0 17.0 (Just "OI")
                    ]
                )
            ]
        )
    ,
        ( "Multiple courses, with lecture, practical and tutorial sections"
        , "{\"payload\": {\"pageableCourse\": {\"courses\": [\
          \{\"code\": \"CHM217H1\", \"sectionCode\": \"F\", \"sections\": [\
          \{\"teachMethod\": \"LEC\", \"sectionNumber\": \"0101\", \"maxEnrolment\": 150, \"currentEnrolment\": 150,\
          \ \"currentWaitlist\": 57, \"instructors\": [{\"firstName\": \"David\", \"lastName\": \"Stone\"}],\
          \ \"meetingTimes\": [\
          \{\"start\": {\"day\": 2, \"millisofday\": 57600000}, \"end\": {\"millisofday\": 61200000},\
          \ \"building\": {\"buildingCode\": \"SS\"}, \"sessionCode\": \"20269\"},\
          \{\"start\": {\"day\": 4, \"millisofday\": 57600000}, \"end\": {\"millisofday\": 61200000},\
          \ \"building\": {\"buildingCode\": \"SS\"}, \"sessionCode\": \"20269\"},\
          \{\"start\": {\"day\": 5, \"millisofday\": 54000000}, \"end\": {\"millisofday\": 57600000},\
          \ \"building\": {\"buildingCode\": \"SS\"}, \"sessionCode\": \"20269\"}]},\
          \{\"teachMethod\": \"PRA\", \"sectionNumber\": \"0101\", \"maxEnrolment\": 40, \"currentEnrolment\": 40,\
          \ \"meetingTimes\": [{\"start\": {\"day\": 1, \"millisofday\": 32400000},\
          \ \"end\": {\"millisofday\": 46800000}, \"building\": {\"buildingCode\": \"LM\"},\
          \ \"sessionCode\": \"20269\"}]},\
          \{\"teachMethod\": \"TUT\", \"sectionNumber\": \"0101\", \"maxEnrolment\": 32, \"currentEnrolment\": 32,\
          \ \"meetingTimes\": [{\"start\": {\"day\": 2, \"millisofday\": 54000000},\
          \ \"end\": {\"millisofday\": 57600000}, \"building\": {\"buildingCode\": \"BF\"},\
          \ \"sessionCode\": \"20269\"}]}]},\
          \{\"code\": \"CSC108H1\", \"sectionCode\": \"F\", \"sections\": [\
          \{\"teachMethod\": \"LEC\", \"sectionNumber\": \"0101\", \"maxEnrolment\": 196, \"currentEnrolment\": 196,\
          \ \"currentWaitlist\": 51, \"instructors\": [{\"firstName\": \"Jacqueline\", \"lastName\": \"Smith\"}],\
          \ \"meetingTimes\": [\
          \{\"start\": {\"day\": 1, \"millisofday\": 43200000}, \"end\": {\"millisofday\": 46800000},\
          \ \"building\": {\"buildingCode\": \"MP\"}, \"sessionCode\": \"20269\"},\
          \{\"start\": {\"day\": 3, \"millisofday\": 39600000}, \"end\": {\"millisofday\": 46800000},\
          \ \"building\": {\"buildingCode\": \"AH\"}, \"sessionCode\": \"20269\"}]}]}]}}}"
        ,
            [
                ( Meeting "CHM217" "F" "LEC0101" 150 "David Stone" 150 57 0
                ,
                    [ Time' (Just "20269") 1.0 16.0 17.0 (Just "SS")
                    , Time' (Just "20269") 3.0 16.0 17.0 (Just "SS")
                    , Time' (Just "20269") 4.0 15.0 16.0 (Just "SS")
                    ]
                )
            , (Meeting "CHM217" "F" "PRA0101" 40 "" 40 0 0, [Time' (Just "20269") 0.0 9.0 13.0 (Just "LM")])
            , (Meeting "CHM217" "F" "TUT0101" 32 "" 32 0 0, [Time' (Just "20269") 1.0 15.0 16.0 (Just "BF")])
            ,
                ( Meeting "CSC108" "F" "LEC0101" 196 "Jacqueline Smith" 196 51 0
                ,
                    [ Time' (Just "20269") 0.0 12.0 13.0 (Just "MP")
                    , Time' (Just "20269") 2.0 11.0 13.0 (Just "AH")
                    ]
                )
            ]
        )
    ,
        ( "Section with no meeting times, meeting inserted with no times"
        , "{\"payload\": {\"pageableCourse\": {\"courses\": [\
          \{\"code\": \"CSC299H1\", \"sectionCode\": \"F\", \"sections\": [\
          \{\"teachMethod\": \"LEC\", \"sectionNumber\": \"0101\", \"maxEnrolment\": 9999}]}]}}}"
        , [(Meeting "CSC299" "F" "LEC0101" 9999 "" 0 0 0, [])]
        )
    ,
        ( "Course with no sections, nothing inserted"
        , "{\"payload\": {\"pageableCourse\": {\"courses\": [\
          \{\"code\": \"CSC299H1\", \"sectionCode\": \"F\"}]}}}"
        , []
        )
    ,
        ( "Response with no courses, nothing inserted"
        , "{\"payload\": {\"pageableCourse\": {\"courses\": []}}}"
        , []
        )
    ,
        ( "Response with a section with no teaching method, no meetings of the page inserted"
        , "{\"payload\": {\"pageableCourse\": {\"courses\": [\
          \{\"code\": \"CSC207H1\", \"sectionCode\": \"F\", \"sections\": [\
          \{\"teachMethod\": \"LEC\", \"sectionNumber\": \"0201\", \"maxEnrolment\": 150, \"currentEnrolment\": 150,\
          \ \"currentWaitlist\": 2, \"instructors\": [{\"firstName\": \"Lindsey\", \"lastName\": \"Shorser\"}]},\
          \{\"sectionNumber\": \"0201\"}]}]}}}"
        , []
        )
    ,
        ( "Response with no payload, nothing inserted"
        , "{}"
        , []
        )
    ,
        ( "Response with no courses field, nothing inserted"
        , "{\"payload\": {\"pageableCourse\": {\"page\": 1, \"pageSize\": 300}}}"
        , []
        )
    ,
        ( "Response that is not valid JSON, nothing inserted"
        , "<html>Service unavailable</html>"
        , []
        )
    ]

-- | Run a test case (label, input response body, expected meetings and their times) on the insertCourses function.
runInsertCoursesTest :: (String, BL.ByteString, [(Meeting, [Time'])]) -> TestTree
runInsertCoursesTest (label, respBody, expected) =
    testCase label $ do
        runDb $ do
            clearDatabase
            insertCourses respBody
        meetings <- getMeetings
        assertEqual ("Unexpected meetings inserted for " ++ label) (map fst expected) (map entityVal meetings)
        times <- mapM (getTimesOf . entityKey) meetings
        assertEqual ("Unexpected times inserted for " ++ label) (map snd expected) times

-- | Run all the insertCourses test cases
runInsertCoursesTests :: [TestTree]
runInsertCoursesTests = map runInsertCoursesTest insertCoursesTestCases

-- | The CSC108 fall lecture, used by the tests that call insertCourses twice
initialResponse :: BL.ByteString
initialResponse =
    "{\"payload\": {\"pageableCourse\": {\"courses\": [\
    \{\"code\": \"CSC108H1\", \"sectionCode\": \"F\", \"sections\": [\
    \{\"teachMethod\": \"LEC\", \"sectionNumber\": \"0101\", \"maxEnrolment\": 196, \"currentEnrolment\": 196,\
    \ \"currentWaitlist\": 51, \"instructors\": [{\"firstName\": \"Jacqueline\", \"lastName\": \"Smith\"}],\
    \ \"meetingTimes\": [\
    \{\"start\": {\"day\": 1, \"millisofday\": 43200000}, \"end\": {\"millisofday\": 46800000},\
    \ \"building\": {\"buildingCode\": \"MP\"}, \"sessionCode\": \"20269\"},\
    \{\"start\": {\"day\": 3, \"millisofday\": 39600000}, \"end\": {\"millisofday\": 46800000},\
    \ \"building\": {\"buildingCode\": \"AH\"}, \"sessionCode\": \"20269\"}]}]}]}}}"

-- | The same lecture as 'initialResponse', as it would be re-fetched after its waitlist
--   cleared and the Wednesday meeting time was dropped
updatedResponse :: BL.ByteString
updatedResponse =
    "{\"payload\": {\"pageableCourse\": {\"courses\": [\
    \{\"code\": \"CSC108H1\", \"sectionCode\": \"F\", \"sections\": [\
    \{\"teachMethod\": \"LEC\", \"sectionNumber\": \"0101\", \"maxEnrolment\": 196, \"currentEnrolment\": 196,\
    \ \"instructors\": [{\"firstName\": \"Jacqueline\", \"lastName\": \"Smith\"}],\
    \ \"meetingTimes\": [\
    \{\"start\": {\"day\": 1, \"millisofday\": 43200000}, \"end\": {\"millisofday\": 46800000},\
    \ \"building\": {\"buildingCode\": \"MP\"}, \"sessionCode\": \"20269\"}]}]}]}}}"

-- | Run test on insertCourses to check that inserting the same response twice does not duplicate entries
testInsertCoursesIdempotent :: TestTree
testInsertCoursesIdempotent =
    testCase "insertCourses is called twice with the same response and does not duplicate entries" $ do
        runDb $ do
            clearDatabase
            insertCourses initialResponse
            insertCourses initialResponse
        meetings <- getMeetings
        times <- mapM (getTimesOf . entityKey) meetings
        assertEqual
            "Expected a single meeting after inserting the same response twice"
            [Meeting "CSC108" "F" "LEC0101" 196 "Jacqueline Smith" 196 51 0]
            (map entityVal meetings)
        assertEqual
            "Expected the same times after inserting the same response twice"
            [
                [ Time' (Just "20269") 0.0 12.0 13.0 (Just "MP")
                , Time' (Just "20269") 2.0 11.0 13.0 (Just "AH")
                ]
            ]
            times

-- | Run test on insertCourses to check that an existing meeting is updated and its times replaced
testInsertCoursesUpdatesExisting :: TestTree
testInsertCoursesUpdatesExisting =
    testCase "insertCourses updates an existing meeting and replaces its times" $ do
        runDb $ do
            clearDatabase
            insertCourses initialResponse
            insertCourses updatedResponse
        meetings <- getMeetings
        times <- mapM (getTimesOf . entityKey) meetings
        assertEqual
            "Expected the existing meeting to be updated rather than duplicated"
            [Meeting "CSC108" "F" "LEC0101" 196 "Jacqueline Smith" 196 0 0]
            (map entityVal meetings)
        assertEqual
            "Expected the times of the existing meeting to be replaced"
            [[Time' (Just "20269") 0.0 12.0 13.0 (Just "MP")]]
            times

-- | Test suite for UtsgJsonParser module
test_utsgJsonParser :: TestTree
test_utsgJsonParser =
    withDatabase "UtsgJsonParser tests" $
        runInsertCoursesTests ++ [testInsertCoursesIdempotent, testInsertCoursesUpdatesExisting]
