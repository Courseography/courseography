{-# LANGUAGE DataKinds, DeriveGeneric, DerivingStrategies, EmptyDataDecls, FlexibleContexts,
             FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             QuasiQuotes, StandaloneDeriving, TemplateHaskell, TypeFamilies,
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

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (..), genericToJSON, withObject,
                   (.!=), (.:?))
import Data.Aeson.KeyMap (elems)
import Data.Aeson.Types (Options (..), Parser, defaultOptions)
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.DataType
import Database.Persist.Sqlite (Key, SqlPersistM, entityVal, selectFirst, (==.))
import Database.Persist.TH
import GHC.Generics
import Text.Read (readMaybe)
import WebParsing.ReqParser (parseReqs)

-- | A two-dimensional point.
type Point = (Double, Double)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Department json
    name T.Text
    Primary name
    UniqueName name

Courses
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
    deriving Generic Show

Times
    weekDay Double
    startHour Double
    endHour Double
    meeting MeetingId
    firstRoom T.Text Maybe
    secondRoom T.Text Maybe

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
    deriving Show

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
    deriving Show

Path json
    graph GraphId
    id_ T.Text
    points [Point]
    fill T.Text
    stroke T.Text
    isRegion Bool
    source T.Text
    target T.Text
    deriving Show

Post
    name PostType
    department T.Text
    code T.Text
    --UniquePostCode code
    --Primary code
    description T.Text
    requirements T.Text
    deriving Show Eq

PostCategory
    post PostId
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
|]

-- ** TODO: Remove these extra types and class instances

-- | JSON SVG data
data SvgJSON =
    SvgJSON { texts :: [Text],
              shapes :: [Shape],
              paths :: [Path]
            } deriving (Show, Generic)

data Time' =
  Time' { weekDay' :: Double,
          startHour' :: Double,
          endHour' :: Double,
          firstRoom' :: Maybe T.Text,
          secondRoom' :: Maybe T.Text
        } deriving (Show, Generic)

data Time =
  Time { weekDay :: Double,
          startHour :: Double,
          endHour :: Double,
          firstRoom :: Maybe Location,
          secondRoom :: Maybe Location
        } deriving (Show, Generic)

data Location =
  Location { room :: T.Text,
            bName :: T.Text,
            bCode :: T.Text,
            address :: T.Text,
            postalCode :: T.Text,
            lat :: Double,
            lng :: Double
          } deriving (Show, Generic)

-- | A Meeting with its associated Times.
data MeetTime = MeetTime {meetInfo :: Meeting, timeInfo :: [Time'] }
  deriving (Show, Generic)

data MeetTime' = MeetTime' { meetData :: Meeting, timeData :: [Time] }
  deriving (Show, Generic)

-- | A Course. TODO: remove this data type (it's redundant).
data Course =
    Course { breadth :: Maybe T.Text,
             description :: Maybe T.Text,
             title :: Maybe T.Text,
             prereqString :: Maybe T.Text,
             allMeetingTimes :: Maybe [MeetTime'],
             name :: !T.Text,
             exclusions :: Maybe T.Text,
             distribution :: Maybe T.Text,
             coreqs :: Maybe T.Text,
             videoUrls :: [T.Text]
           } deriving (Show, Generic)

instance ToJSON Course
instance ToJSON Time
instance ToJSON MeetTime'
instance ToJSON Building
instance ToJSON Location

-- instance FromJSON required so that tables can be parsed into JSON,
-- not necessary otherwise.
instance FromJSON SvgJSON

-- JSON encoding/decoding
instance FromJSON Courses where
  parseJSON = withObject "Expected Object for Courses" $ \o -> do
    newCode <- o .:? "code" .!= "CSC???"
    newTitle  <- o .:? "courseTitle"
    newDescription  <- o .:? "courseDescription"
    newPrereqString <- o .:? "prerequisite"
    let newPrereqs = fmap (T.pack . show . parseReqs . T.unpack) newPrereqString
    newExclusions <- o .:? "exclusion"
    newCoreqs <- o .:? "corequisite"
    return $ Courses newCode
                     newTitle
                     newDescription
                     newPrereqs
                     newExclusions
                     Nothing -- breadth
                     Nothing -- distribution
                     Nothing -- (Just prereqString)
                     newCoreqs
                     []

instance ToJSON Meeting where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier =
      (\field -> toLower (head field): tail field) .
      drop 7
  }

instance FromJSON Meeting where
  parseJSON = withObject "Expected Object for Lecture, Tutorial or Practical" $ \o -> do
    teachingMethod :: T.Text <- o .:? "teachingMethod" .!= ""
    sectionNumber :: T.Text <- o .:? "sectionNumber" .!= ""
    let sectionId = T.concat [teachingMethod, sectionNumber]

    capStr <- o .:? "enrollmentCapacity" .!= "-1"
    enrolStr <- o .:? "actualEnrolment" .!= "0"
    waitStr <- o .:? "actualWaitlist" .!= "0"
    let cap = fromMaybe (-1) $ readMaybe capStr
        enrol = fromMaybe 0 $ readMaybe enrolStr
        wait = fromMaybe 0 $ readMaybe waitStr
    instrMap2 :: Value <- o .:? "instructors" .!= Null
    let instrList =
          case instrMap2 of
            Object obj -> elems obj
            _ -> []

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
    meetingDayStr <- o .:? "meetingDay"
    meetingStartTimeStr <- o .:? "meetingStartTime"
    meetingEndTimeStr <- o .:? "meetingEndTime"
    meetingRoom1 <- o .:? "assignedRoom1" .!= Nothing
    meetingRoom2 <- o .:? "assignedRoom2" .!= Nothing
    let (meetingDay, meetingStartTime, meetingEndTime) = getTimeVals meetingDayStr meetingStartTimeStr meetingEndTimeStr
    return $ Time' meetingDay meetingStartTime meetingEndTime meetingRoom1 meetingRoom2

instance FromJSON MeetTime where
  parseJSON (Object o) = do
    meeting <- parseJSON (Object o)
    timeMap :: HM.HashMap T.Text Time' <- o .:? "schedule" .!= HM.empty <|> return HM.empty
    return $ MeetTime meeting (HM.elems timeMap)
  parseJSON _ = fail "Invalid meeting"

-- | Helpers for parsing JSON
parseInstr :: Value -> Parser T.Text
parseInstr (Object io) = do
  firstName <- io .:? "firstName" .!= ""
  lastName <- io .:? "lastName" .!= ""
  return (T.concat [firstName, ". ", lastName])
parseInstr _ = return ""

-- | Converts 24-hour time into a double
-- | Assumes times are rounded to the nearest hour
getHourVal :: String -> Double
getHourVal time = (read $ take 2 time :: Double) + (/) (read $ drop 3 time :: Double) 60

-- | Converts a weekday into a double
-- | Monday to Friday becomes 0.0 to 4.0
getDayVal :: String -> Double
getDayVal "MO" = 0.0
getDayVal "TU" = 1.0
getDayVal "WE" = 2.0
getDayVal "TH" = 3.0
getDayVal "FR" = 4.0
getDayVal _    = 4.0

-- | Convert the given day, start time and end time to a tuple of Doubles. If nothing is given,
--   the place holder is 5 and 25, indicating the day and times are invalid.
getTimeVals :: Maybe String -> Maybe String -> Maybe String -> (Double, Double, Double)
getTimeVals (Just day) (Just start) (Just end) = do
    let dayDbl = getDayVal day
        startDbl = getHourVal start
        endDbl = getHourVal end
    (dayDbl, startDbl, endDbl)
getTimeVals _ _ _ = (5.0, 25.0, 25.0)

-- | Convert Times into Time
buildTime :: Times -> SqlPersistM Time
buildTime t = do
  room1 <- buildLocation (timesFirstRoom t)
  room2 <- buildLocation (timesSecondRoom t)
  return $ Time (timesWeekDay t)
    (timesStartHour t)
    (timesEndHour t)
    room1
    room2

buildTimes :: Key Meeting -> Time' -> Times
buildTimes meetingKey t =
  Times (weekDay' t)
    (startHour' t)
    (endHour' t)
    meetingKey
    (firstRoom' t)
    (secondRoom' t)

buildLocation :: Maybe T.Text -> SqlPersistM (Maybe Location)
buildLocation rm = do
  case rm of
    Nothing -> return Nothing
    Just r -> do
      maybeEntityBuilding <- selectFirst [BuildingCode ==. T.take 2 r] []
      case maybeEntityBuilding of
        Nothing -> return Nothing
        Just entBuilding -> do
          let building = entityVal entBuilding
          return $ Just $ Location r
                                  (buildingName building)
                                  (buildingCode building)
                                  (buildingAddress building)
                                  (buildingPostalCode building)
                                  (buildingLat building)
                                  (buildingLng building)
