{-# LANGUAGE DeriveGeneric #-}

module Models.Meeting (
    MeetingData (..),
    MeetingParsedData (..),
    insertMeetingParsedData,
    meetingQuery,
    buildMeetTimes,
    returnMeeting,
    getMeetingTime,
    getMeetingSection,
) where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON, Value (Object), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Maybe (fromJust)
import qualified Data.Text as T (Text, append, concat, intercalate, isPrefixOf, null, tail, take, toUpper)
import Database.Persist.Sqlite (
    Entity,
    SqlPersistM,
    Update,
    deleteWhere,
    entityKey,
    entityVal,
    insert,
    insertMany_,
    selectFirst,
    selectList,
    upsert,
    (<-.),
    (=.),
    (==.),
 )
import Database.Tables as Tables
import Models.Time (TimeData, buildTime)

import GHC.Generics

-- | The data for a single meeting section, as returned by the back-end to the front-end.
-- This is different from the schema-defined 'Meeting' type (in "Database.Tables").
-- A single meeting section (such as a lecture, tutorial, practical etc) often meets at multiple times and locations
-- throughout the week, so 'MeetingData' bundles the section's 'Meeting' information with
-- its associated list of 'TimeData' records for JSON serialization to the client.
data MeetingData = MeetingData {meetData :: Meeting, timeData :: [TimeData]}
    deriving (Show, Generic)

instance ToJSON MeetingData

-- | A single meeting section parsed from the timetable API, before it is inserted into the database.
-- Its 'Time' can't be built until the 'Meeting' has been inserted and has a 'MeetingId',
-- so they are stored as functions awaiting that key.
data MeetingParsedData = MeetingParsedData {meetInfo :: Meeting, timeInfo :: [MeetingId -> Time]}

instance FromJSON MeetingParsedData where
    parseJSON = withObject "Invalid meeting" $ \o -> do
        meeting <- parseMeeting o
        rawTimes :: [Value] <- o .:? "meetingTimes" .!= []
        timeFunctionList <- mapM parseTime rawTimes
        return $ MeetingParsedData meeting timeFunctionList
      where
        -- Parse a lecture, tutorial or practical. Its code and session are left empty,
        -- since they come from the enclosing course.
        parseMeeting :: Object -> Parser Meeting
        parseMeeting o = do
            teachingMethod :: T.Text <- o .:? "teachMethod" .!= ""
            sectionNumber :: T.Text <- o .:? "sectionNumber" .!= ""
            let sectionId = T.concat [teachingMethod, sectionNumber]

            cap <- o .:? "maxEnrolment" .!= (-1)
            enrol <- o .:? "currentEnrolment" .!= 0
            wait <- o .:? "currentWaitlist" .!= 0
            instrList <- o .:? "instructors" .!= []
            instrs <- mapM parseInstr instrList

            let extra = 0
            let instructor = T.intercalate "; " $ filter (not . T.null) instrs
            if teachingMethod == "LEC" || teachingMethod == "TUT" || teachingMethod == "PRA"
                then
                    return $ Meeting "" "" sectionId cap instructor enrol wait extra
                else
                    fail "Not a lecture, Tutorial or Practical"

        -- Parse an instructor's full name
        parseInstr :: Value -> Parser T.Text
        parseInstr (Object obj) = do
            firstName <- obj .:? "firstName" .!= ""
            lastName <- obj .:? "lastName" .!= ""
            return (T.concat [firstName, " ", lastName])
        parseInstr _ = return ""

        parseTime :: Value -> Parser (MeetingId -> Time)
        parseTime = withObject "Expected Object for Time" $ \o -> do
            startObject <- o .: "start"
            endObject <- o .: "end"
            meetingDay :: Maybe Int <- startObject .:? "day" .!= Nothing
            meetingStartTime :: Maybe Int <- startObject .:? "millisofday" .!= Nothing
            meetingEndTime :: Maybe Int <- endObject .:? "millisofday" .!= Nothing

            building <- o .: "building"
            buildingCode' <- building .: "buildingCode"

            session <- o .: "sessionCode"

            let (adjustedDay, adjustedStartTime, adjustedEndTime) = convertTimeVals meetingDay meetingStartTime meetingEndTime
            return $ \meetingId -> Time session adjustedDay adjustedStartTime adjustedEndTime meetingId buildingCode'

        -- Converts the miliseconds time into hourly time
        -- Assumes times are rounded to the nearest hour
        getHourVal :: Int -> Double
        getHourVal millis =
            let
                seconds = fromIntegral millis / 1000.0
                minutes = seconds / 60
                hours = minutes / 60
             in
                hours

        -- Converts a the given day into a double representation for the database
        -- Monday (1) to Friday (5) becomes 0.0 to 4.0
        getDayVal :: Int -> Double
        getDayVal 1 = 0.0
        getDayVal 2 = 1.0
        getDayVal 3 = 2.0
        getDayVal 4 = 3.0
        getDayVal _ = 4.0

        -- Convert the given day, start time and end time to a tuple of Doubles. If nothing is given,
        -- the place holder is 5 and 25, indicating the day and times are invalid.
        convertTimeVals :: Maybe Int -> Maybe Int -> Maybe Int -> (Double, Double, Double)
        convertTimeVals (Just day) (Just start) (Just end) =
            let dayDbl = getDayVal day
                startDbl = getHourVal start
                endDbl = getHourVal end
             in (dayDbl, startDbl, endDbl)
        convertTimeVals _ _ _ = (5.0, 25.0, 25.0)

-- | Insert or update a meeting and then delete
--   and re-insert the corresponding Times into the database.
insertMeetingParsedData :: MeetingParsedData -> SqlPersistM ()
insertMeetingParsedData (MeetingParsedData meetingData meetingTimeFunctions) = do
    -- Check if the meeting already exists in the meeting table
    let code = meetingCode meetingData
    let session = meetingSession meetingData
    let section = meetingSection meetingData
    maybeMeetingKey <- selectFirst [MeetingCode ==. code, MeetingSession ==. session, MeetingSection ==. section] []
    case maybeMeetingKey of
        Just _ -> do
            -- meeting already exists, so update/replace
            entity <- upsert meetingData (meetingUpdates meetingData)
            let meetingKey = entityKey entity
            deleteWhere [TimeMeeting ==. meetingKey]
            let allTimes = map ($ meetingKey) meetingTimeFunctions
            insertMany_ allTimes
        Nothing -> do
            -- meeting does not exist, so insert
            meetingKey <- insert meetingData
            let allTimes = map ($ meetingKey) meetingTimeFunctions
            insertMany_ allTimes
  where
    -- Update the entries of the Meeting Table if necessary
    meetingUpdates :: Meeting -> [Update Meeting]
    meetingUpdates m =
        [ MeetingCode =. meetingCode m
        , MeetingSession =. meetingSession m
        , MeetingSection =. meetingSection m
        , MeetingCap =. meetingCap m
        , MeetingInstructor =. meetingInstructor m
        , MeetingEnrol =. meetingEnrol m
        , MeetingWait =. meetingWait m
        , MeetingExtra =. meetingExtra m
        ]

-- | Queries the database for all matching lectures, tutorials,
meetingQuery :: [T.Text] -> SqlPersistM [MeetingData]
meetingQuery meetingCodes = do
    allMeetings <- selectList [MeetingCode <-. map (T.take 6) meetingCodes] []
    mapM buildMeetTimes allMeetings

-- | Queries the database for all times corresponding to a given meeting.
buildMeetTimes :: Entity Meeting -> SqlPersistM MeetingData
buildMeetTimes meet = do
    allTimes :: [Entity Time] <- selectList [TimeMeeting ==. entityKey meet] []
    parsedTime <- mapM (buildTime . entityVal) allTimes
    return $ MeetingData (entityVal meet) parsedTime

-- | Queries the database for all information regarding a specific meeting for
--  a course, returns a Meeting.
returnMeeting :: T.Text -> T.Text -> T.Text -> SqlPersistM (Maybe (Entity Meeting))
returnMeeting lowerStr sect session = do
    selectFirst
        [ MeetingCode ==. T.toUpper lowerStr
        , MeetingSection ==. sect
        , MeetingSession ==. session
        ]
        []

-- | Queries the database for all times regarding a specific meeting (lecture, tutorial or practial) for
-- a course, returns a list of TimeData.
getMeetingTime :: (T.Text, T.Text, T.Text) -> SqlPersistM [TimeData]
getMeetingTime (meetingCode_, meetingSection_, meetingSession_) = do
    maybeEntityMeetings <-
        selectFirst
            [ MeetingCode ==. meetingCode_
            , MeetingSection ==. getMeetingSection meetingSection_
            , MeetingSession ==. meetingSession_
            ]
            []
    allTimes <- selectList [TimeMeeting ==. entityKey (fromJust maybeEntityMeetings)] []
    mapM (buildTime . entityVal) allTimes

getMeetingSection :: T.Text -> T.Text
getMeetingSection sec
    | T.isPrefixOf "L" sec = T.append "LEC" sectCode
    | T.isPrefixOf "T" sec = T.append "TUT" sectCode
    | T.isPrefixOf "P" sec = T.append "PRA" sectCode
    | otherwise = sec
  where
    sectCode = T.tail sec
