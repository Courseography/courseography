{-# LANGUAGE DeriveGeneric,
             EmptyDataDecls,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             QuasiQuotes,
             TemplateHaskell,
             TypeFamilies #-}

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

import Database.Persist.TH
import Database.DataType
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Aeson ((.:?), (.!=), FromJSON(parseJSON), ToJSON(toJSON), Value(..), genericToJSON, withObject)
import Data.Aeson.Types (Parser, defaultOptions, Options(..))
import GHC.Generics
import WebParsing.ReqParser (parseReqs)
import Control.Applicative ((<|>))


-- | A data type representing a time for the section of a course.
-- The record is comprised of three fields: weekDay (represented as a number
-- of the week), startHour (the start time of the lecture) and endHour (the end time of the lecture).
-- The dates span Monday-Friday, being represented by 0-4 respectively.
-- The start time and end time are numbers between 0-23.
data Time = Time { weekDay :: Double, startHour :: Double, endHour :: Double } deriving (Show, Read, Eq, Generic)
derivePersistField "Time"

data Room = Room { roomField :: (T.Text, T.Text)} deriving (Show, Read, Eq, Generic)
derivePersistField "Room"


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
    manualTutorialEnrolment Bool Maybe
    manualPracticalEnrolment Bool Maybe
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
    times [Time]
    cap Int
    instructor T.Text
    enrol Int
    wait Int
    extra Int
    timeStr T.Text
    room [Room]
    deriving Generic Show

Times
    weekDay Double
    startHour Double
    endHour Double
    meeting MeetingId Maybe
    firstRoom T.Text Maybe
    secondRoom T.Text Maybe
    deriving Generic Show

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
    deriving Show

PostCategory
    post PostId
    name T.Text
    deriving Show
|]

-- ** TODO: Remove these extra types and class instances

-- | JSON SVG data
data SvgJSON =
    SvgJSON { texts :: [Text],
              shapes :: [Shape],
              paths :: [Path]
            } deriving (Show, Generic)

-- change session to be lectures :: [(Meeting, [Times])], or perhaps MeetingTimes {meeting:: Meeting, times :; [Times]}
data Session =
    Session { lectures :: [Meeting],
              tutorials :: [Meeting],
              practicals :: [Meeting]
            } deriving (Show, Generic)

-- | A Course. TODO: remove this data type (it's redundant).
data Course =
    Course { breadth :: Maybe T.Text,
             description :: Maybe T.Text,
             title :: Maybe T.Text,
             prereqString :: Maybe T.Text,
             fallSession :: Maybe Session,
             springSession :: Maybe Session,
             yearSession :: Maybe Session,
             name :: !T.Text,
             exclusions :: Maybe T.Text,
             manualTutorialEnrolment :: Maybe Bool,
             manualPracticalEnrolment :: Maybe Bool,
             distribution :: Maybe T.Text,
             coreqs :: Maybe T.Text,
             videoUrls :: [T.Text]
           } deriving (Show, Generic)

instance ToJSON Course
instance ToJSON Session
instance ToJSON Time
instance ToJSON Room
instance ToJSON MeetTimes

-- instance FromJSON required so that tables can be parsed into JSON,
-- not necessary otherwise.
instance FromJSON SvgJSON

-- | Converts a Time to a T.Text.
-- This removes the period from the double, as the JavaScript code,
-- uses the output in an element's ID, which is then later used in
-- jQuery. @.@ is a jQuery meta-character, and must be removed from the ID.
convertTimeToString :: Time -> [T.Text]
convertTimeToString (Time day startNum endNum) =
  [T.pack . show $ (floor day :: Int),
   T.replace "." "-" . T.pack . show $ (show startNum ++ "-" ++ show endNum)]

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
                     (Just False)
                     (Just False)
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

instance ToJSON Times where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier =
      (\field -> toLower (head field): tail field) .
      drop 7
  }

instance FromJSON Meeting where
  parseJSON = withObject "Expected Object for Lecture, Tutorial or Practical" $ \o -> do
    teachingMethod :: T.Text <- o .:? "teachingMethod" .!= ""
    sectionNumber :: T.Text <- o .:? "sectionNumber" .!= ""
    timeMap :: Value <- o .:? "schedule" .!= Null
    (allTimes, allRooms) <- case timeMap of
        Object obj -> do
            timesAndRooms <- mapM parseSchedules (HM.elems obj)
            return (concatMap fst timesAndRooms, concatMap snd timesAndRooms)
        _ -> return ([], [])
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
            Object obj -> HM.elems obj
            _ -> []

    instrs <- mapM parseInstr instrList
    let extra = 0
    let timeStr = ""
    let instructor = T.intercalate "; " $ filter (not . T.null) instrs
    if teachingMethod == "LEC" || teachingMethod == "TUT" || teachingMethod == "PRA"
    then
      return $ Meeting "" "" sectionId allTimes cap instructor enrol wait extra timeStr allRooms
    else
      fail "Not a lecture, Tutorial or Practical"

instance FromJSON Times where
  parseJSON = withObject "Expected Object for Times" $ \o -> do
    meetingDayStr <- o .:? "meetingDay" .!= "5.0"
    meetingStartTimeStr <- o .:? "meetingStartTime" .!= "5.0"
    meetingEndTimeStr <- o .:? "meetingEndTime" .!= "5.0"
    meetingRoom1 <- o .:? "assignedRoom1" .!= Nothing
    meetingRoom2 <- o .:? "assignedRoom2" .!= Nothing
    let meetingDay = fromMaybe 5.0 (readMaybe meetingDayStr)
        meetingStartTime = fromMaybe 5.0 (readMaybe meetingStartTimeStr)
        meetingEndTime = fromMaybe 5.0 (readMaybe meetingEndTimeStr)
    return $ Times meetingDay meetingStartTime meetingEndTime Nothing meetingRoom1 meetingRoom2

data MeetTimes = MeetTimes { meetingData :: Meeting, timesData :: [Times] }
  deriving (Show, Generic)

instance FromJSON MeetTimes where
  parseJSON (Object o) = do
    meeting <- parseJSON (Object o)
    timeMap :: HM.HashMap T.Text Times <- o .:? "schedule" .!= HM.empty <|> return HM.empty
    return $ MeetTimes meeting (HM.elems timeMap)
  parseJSON _ = fail "Invalid meeting"

-- | Helpers for parsing JSON
parseInstr :: Value -> Parser T.Text
parseInstr (Object io) = do
  firstName <- io .:? "firstName" .!= ""
  lastName <- io .:? "lastName" .!= ""
  return (T.concat [firstName, ". ", lastName])
parseInstr _ = return ""

parseSchedules :: Value -> Parser ([Time], [Room])
parseSchedules (Object obj) = do
    meetingDay <- obj .:? "meetingDay"
    meetingStartTime <- obj .:? "meetingStartTime"
    meetingEndTime <- obj .:? "meetingEndTime"
    meetingRoom1 <- obj .:? "assignedRoom1" .!= ""
    meetingRoom2 <- obj .:? "assignedRoom2" .!= ""
    let times = getTimeSlots meetingDay meetingStartTime meetingEndTime
        rooms = replicate (length times) (Room (meetingRoom1, meetingRoom2))
    return (times, rooms)
parseSchedules _ = return ([], [])

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

-- | Takes a day and start/end times then generates a Time with the day and start/end times
getTimeSlots :: Maybe String -> Maybe String -> Maybe String -> [Time]
getTimeSlots (Just day) (Just start) (Just end) = do
    let dayDbl = getDayVal day
        startDbl = getHourVal start
        endDbl = getHourVal end
    [Time dayDbl startDbl endDbl]
getTimeSlots _ _ _ = []
