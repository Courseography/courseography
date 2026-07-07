{-# LANGUAGE DataKinds, DeriveGeneric, DerivingStrategies, EmptyDataDecls, FlexibleContexts,
             FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             QuasiQuotes, StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-|
    Module      : Database.Tables
    Description : The database schema (and some helpers).

This module defines the database schema. It uses Template Haskell to also
create new types for these values so that they can be used in the rest of
the application.

Though types and typeclass instances are created automatically, we currently
have a few manually-generated spots to clean up. This should be rather
straightforward.
-}

module Database.Tables where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), genericToJSON, withObject, (.!=), (.:),
                   (.:?))
import Data.Aeson.Types (Options (..), Parser, Value (Object), defaultOptions)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.DataType
import Database.Persist.TH
import GHC.Generics

-- | A two-dimensional point.
type Point = (Double, Double)
-- | A matrix of any dimensions.
type Matrix = [[Double]]
-- | A vector of any dimesions.
type Vector = [Double]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Department json
    name T.Text
    Primary name
    UniqueName name

Course
    code T.Text
    Primary code
    title T.Text Maybe
    description T.Text Maybe
    prereqs T.Text Maybe
    exclusions T.Text Maybe
    breadth BreadthId Maybe
    distribution DistributionId Maybe
    prereqString T.Text Maybe
    coreqs T.Text Maybe
    videoUrls [T.Text]
    deriving Show

Meeting
    code T.Text
    session T.Text
    section T.Text
    cap Int
    instructor T.Text
    enrol Int
    wait Int
    extra Int
    deriving Generic Show Eq
    UniqueMeeting code session section

Times
    session T.Text Maybe
    weekDay Double
    startHour Double
    endHour Double
    meeting MeetingId
    location T.Text Maybe

Breadth
    description T.Text
    deriving Show

Distribution
    description T.Text
    deriving Show

Graph json
    title T.Text
    width Double
    height Double
    dynamic Bool
    deriving Show

Text json
    graph GraphId
    rId T.Text
    pos Point
    text T.Text
    align T.Text
    fill T.Text
    deriving Show Eq
    transform [Double] default=[1,0,0,1,0,0]

Shape json
    graph GraphId
    id_ T.Text
    pos Point
    width Double
    height Double
    fill T.Text
    stroke T.Text
    text [Text]
    type_ ShapeType
    deriving Show Eq
    transform [Double] default=[1,0,0,1,0,0]

Path json
    graph GraphId
    id_ T.Text
    points [Point]
    fill T.Text
    stroke T.Text
    isRegion Bool
    source T.Text
    target T.Text
    deriving Show Eq
    transform [Double] default=[1,0,0,1,0,0]

Program
    name ProgramType
    department T.Text
    code T.Text
    --UniqueProgramCode code
    --Primary code
    description T.Text
    requirements T.Text
    created UTCTime
    modified UTCTime
    deriving Show Eq Generic

ProgramCategory
    program ProgramId
    name T.Text
    deriving Show

Building
    code T.Text
    name T.Text
    address T.Text
    postalCode T.Text
    lat Double
    lng Double
    deriving Generic Show

SchemaVersion
    version Int
    deriving Show Eq
|]

-- ** TODO: Remove these extra types and class instances

data Time' =
  Time' { timeSession' :: Maybe T.Text,
          weekDay' :: Double,
          startHour' :: Double,
          endHour' :: Double,
          timeLocation' :: Maybe T.Text
        } deriving (Show, Eq, Generic)

data Time =
  Time { timeSession :: Maybe T.Text,
         weekDay :: Double,
         startHour :: Double,
         endHour :: Double,
         timeLocation :: Maybe Building
       } deriving (Show, Generic)

-- | A Meeting with its associated Times.
data MeetTime = MeetTime {meetInfo :: Meeting, timeInfo :: [Time'] }
  deriving (Show, Generic)

data MeetTime' = MeetTime' { meetData :: Meeting, timeData :: [Time] }
  deriving (Show, Generic)

instance ToJSON Program
instance ToJSON Time
instance ToJSON MeetTime'
instance ToJSON Building

instance ToJSON Meeting where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier =
        lowerFirst .
        drop 7
    }
    where
      lowerFirst :: [Char] -> String
      lowerFirst [] = ""
      lowerFirst (fieldHead: fieldTail) = toLower fieldHead: fieldTail

instance FromJSON Meeting where
  parseJSON = withObject "Expected Object for Lecture, Tutorial or Practical" $ \o -> do
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

instance FromJSON Time' where
  parseJSON = withObject "Expected Object for Times" $ \o -> do
    startObject <- o .: "start"
    endObject <- o .: "end"
    meetingDay :: Maybe Int <- startObject .:? "day" .!= Nothing
    meetingStartTime :: Maybe Int <- startObject .:? "millisofday" .!= Nothing
    meetingEndTime :: Maybe Int <- endObject .:? "millisofday" .!= Nothing

    building <- o .: "building"
    buildingCode <- building .: "buildingCode"

    session <- o .:? "sessionCode"

    let (adjustedDay, adjustedStartTime, adjustedEndTime) = convertTimeVals meetingDay meetingStartTime meetingEndTime
    return $ Time' session adjustedDay adjustedStartTime adjustedEndTime buildingCode

instance FromJSON MeetTime where
  parseJSON (Object o) = do
    meeting <- parseJSON (Object o)
    timesList :: [Time'] <- o .:? "meetingTimes" .!= []
    return $ MeetTime meeting timesList
  parseJSON _ = fail "Invalid meeting"

-- | Helpers for parsing JSON
parseInstr :: Value -> Parser T.Text
parseInstr (Object io) = do
  firstName <- io .:? "firstName" .!= ""
  lastName <- io .:? "lastName" .!= ""
  return (T.concat [firstName, " ", lastName])
parseInstr _ = return ""

-- | Converts the miliseconds time into hourly time
-- | Assumes times are rounded to the nearest hour
getHourVal :: Int -> Double
getHourVal millis =
  let
    seconds = fromIntegral millis / 1000.0
    minutes = seconds / 60
    hours = minutes / 60
  in
    hours

-- | Converts a the given day into a double representation for the database
-- | Monday (1) to Friday (5) becomes 0.0 to 4.0
getDayVal :: Int -> Double
getDayVal 1 = 0.0
getDayVal 2 = 1.0
getDayVal 3 = 2.0
getDayVal 4 = 3.0
getDayVal 5 = 4.0
getDayVal _ = 4.0

-- | Convert the given day, start time and end time to a tuple of Doubles. If nothing is given,
--   the place holder is 5 and 25, indicating the day and times are invalid.
convertTimeVals :: Maybe Int -> Maybe Int -> Maybe Int -> (Double, Double, Double)
convertTimeVals (Just day) (Just start) (Just end) =
    let dayDbl = getDayVal day
        startDbl = getHourVal start
        endDbl = getHourVal end
    in (dayDbl, startDbl, endDbl)
convertTimeVals _ _ _ = (5.0, 25.0, 25.0)
