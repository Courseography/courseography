{-# LANGUAGE DeriveGeneric #-}

module Models.Meeting (
    MeetingData (..),
    meetingQuery,
    buildMeetTimes,
    returnMeeting,
    getMeetingTime,
    getMeetingSection,
) where

import Data.Aeson (ToJSON)

import Data.Maybe (fromJust)
import qualified Data.Text as T (Text, append, isPrefixOf, tail, take, toUpper)
import Database.Persist.Sqlite (
    Entity,
    SqlPersistM,
    entityKey,
    entityVal,
    selectFirst,
    selectList,
    (<-.),
    (==.),
 )
import Database.Tables as Tables
import Models.Time (TimeData, buildTime)

import GHC.Generics

data MeetingData = MeetingData {meetData :: Meeting, timeData :: [TimeData]}
    deriving (Show, Generic)

instance ToJSON MeetingData

-- | Queries the database for all matching lectures, tutorials,
meetingQuery :: [T.Text] -> SqlPersistM [MeetingData]
meetingQuery meetingCodes = do
    allMeetings <- selectList [MeetingCode <-. map (T.take 6) meetingCodes] []
    mapM buildMeetTimes allMeetings

-- | Queries the database for all times corresponding to a given meeting.
buildMeetTimes :: Entity Meeting -> SqlPersistM MeetingData
buildMeetTimes meet = do
    allTimes :: [Entity Times] <- selectList [TimesMeeting ==. entityKey meet] []
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
    allTimes <- selectList [TimesMeeting ==. entityKey (fromJust maybeEntityMeetings)] []
    mapM (buildTime . entityVal) allTimes

getMeetingSection :: T.Text -> T.Text
getMeetingSection sec
    | T.isPrefixOf "L" sec = T.append "LEC" sectCode
    | T.isPrefixOf "T" sec = T.append "TUT" sectCode
    | T.isPrefixOf "P" sec = T.append "PRA" sectCode
    | otherwise = sec
  where
    sectCode = T.tail sec
