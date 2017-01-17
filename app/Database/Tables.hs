{-# LANGUAGE EmptyDataDecls,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             OverloadedStrings,
             DeriveGeneric,
             QuasiQuotes,
             ScopedTypeVariables,
             TemplateHaskell,
             TypeFamilies #-}

{-|
Description: The database schema (and some helpers).

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
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Aeson ((.:?), (.!=), FromJSON(parseJSON), ToJSON(toJSON), Value(..), genericToJSON, withObject)
import Data.Aeson.Types (Parser, defaultOptions, Options(..))
import GHC.Generics
import WebParsing.PrerequisiteParsing

-- | A data type representing a time for the section of a course.
-- The first list is comprised of two values: the date (represented as a number
-- of the week), and the time. The dates span Monday-Friday, being represented
-- by 1-5 respectively. The time is a number between 0-23.
-- TODO: Change this datatype. This datatype shouldn't be implemented with
-- a list, perhaps a tuple would be better.
data Time = Time { timeField :: [Double] } deriving (Show, Read, Eq, Generic)
derivePersistField "Time"

-- | A two-dimensional point.
type Point = (Double, Double)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Department json
    code [T.Text]
    name T.Text

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

Lecture
    code T.Text
    Foreign Courses fkcourse code
    session T.Text
    section T.Text
    times [Time]
    cap Int
    instructor T.Text
    enrol Int
    wait Int
    extra Int
    timeStr T.Text
    deriving Generic Show

Tutorial
    code T.Text
    section T.Text Maybe
    session T.Text
    times [Time]
    deriving Generic Show

Breadth
    description String
    deriving Show

Distribution
    description String
    deriving Show

Graph json
    title String
    width Double
    height Double
    deriving Show

Text json
    graph GraphId
    rId String
    pos Point
    text String
    align String
    fill String
    deriving Show

Shape json
    graph GraphId
    id_ String
    pos Point
    width Double
    height Double
    fill String
    stroke String
    text [Text]
    tolerance Double
    type_ ShapeType
    deriving Show

Path json
    graph GraphId
    id_ String
    points [Point]
    fill String
    stroke String
    isRegion Bool
    source String
    target String
    deriving Show

FacebookTest
    fId String
    testString String
    deriving Show

Post
    name T.Text
    department T.Text
    code T.Text
    description T.Text
    deriving Show

PostCategory
    name T.Text
    postCode T.Text
    deriving Show
|]

-- ** TODO: Remove these extra types and class instances

-- | JSON SVG data
data SvgJSON =
    SvgJSON { texts :: [Text],
              shapes :: [Shape],
              paths :: [Path]
            } deriving (Show, Generic)

-- | A Session.
data Session =
    Session { lectures :: [Lecture],
              tutorials :: [Tutorial]
            } deriving (Show, Generic)

-- | A Course.
-- each element of prereqs can be one of three things:
--
--     * a one-element list containing a course code
--     * a list starting with "and", and 2 or more course codes
--     * a list starting with "or", and 2 or more course codes
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
             prereqs :: Maybe T.Text,
             coreqs :: Maybe T.Text,
             videoUrls :: [T.Text]
           } deriving (Show, Generic)

instance ToJSON Course
instance ToJSON Session
instance ToJSON Time

-- instance FromJSON required so that tables can be parsed into JSON,
-- not necessary otherwise.
instance FromJSON SvgJSON

-- | Converts a Double to a T.Text.
-- This removes the period from the double, as the JavaScript code,
-- uses the output in an element's ID, which is then later used in
-- jQuery. @.@ is a jQuery meta-character, and must be removed from the ID.
convertTimeToString :: Time -> [T.Text]
convertTimeToString (Time [day, timeNum]) =
  [T.pack . show . floor $ day,
   T.replace "." "-" . T.pack . show $ timeNum]


-- JSON encoding/decoding
instance FromJSON Courses where
  parseJSON = withObject "Expected Object for Courses" $ \o -> do
    newCode <- o .:? "code" .!= "CSC???"
    newTitle  <- o .:? "courseTitle"
    newDescription  <- o .:? "courseDescription"
    newPrereqString <- o .:? "prerequisite"
    let newPrereqs = parsePrerequisites newPrereqString
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

instance ToJSON Lecture where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier =
      (\field -> (toLower $ head field): (tail field)) .
      drop 7
  }

instance FromJSON Lecture where
  parseJSON = withObject "Expected Object for Lecture" $ \o -> do
    teachingMethod :: T.Text <- o .:? "teachingMethod" .!= ""
    sectionNumber :: T.Text <- o .:? "sectionNumber" .!= ""
    timeMap :: Value <- o .:? "schedule" .!= Null
    allTimes <- case timeMap of
        Object obj -> do
            times <- mapM parseTimes (HM.elems obj)
            return $ concat times
        _ -> return []
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
    if teachingMethod == "LEC"
    then
      return $ Lecture "" "" sectionId allTimes cap instructor enrol wait extra timeStr
    else
      fail "Not a lecture"

instance ToJSON Tutorial where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier =
      (\field -> (toLower $ head field): (tail field)) .
      drop 8
  }

instance FromJSON Tutorial where
  parseJSON = withObject "Expected Object for Tutorial" $ \o -> do
    teachingMethod :: T.Text <- o .:? "teachingMethod" .!= ""
    sectionNumber :: T.Text <- o .:? "sectionNumber" .!= ""
    timeMap :: Value <- o .:? "schedule" .!= Null
    allTimes <- case timeMap of
        Object obj -> do
            times <- mapM parseTimes (HM.elems obj)
            return $ concat times
        _ -> return []
    let sectionId = T.concat [teachingMethod, sectionNumber]

    -- TODO: Tutorials should have these stats, too!
    -- capStr <- o .:? "enrollmentCapacity" .!= "-1"
    -- enrolStr <- o .:? "actualEnrolment" .!= "0"
    -- waitStr <- o .:? "actualWaitlist" .!= "0"
    -- let cap = fromMaybe (-1) $ readMaybe capStr
    --     enrol = fromMaybe 0 $ readMaybe enrolStr
    --     wait = fromMaybe 0 $ readMaybe waitStr
    if teachingMethod == "TUT"
    then
      return $ Tutorial "" (Just sectionId) "" allTimes
    else
      fail "Not a tutorial"


-- | Helpers for parsing JSON
parseInstr :: Value -> Parser T.Text
parseInstr (Object io) = do
  firstName <- io .:? "firstName" .!= ""
  lastName <- io .:? "lastName" .!= ""
  return (T.concat [firstName, ". ", lastName])
parseInstr _ = return ""

parseTimes :: Value -> Parser [Time]
parseTimes (Object obj) = do
    meetingDay <- obj .:? "meetingDay"
    meetingStartTime <- obj .:? "meetingStartTime"
    meetingEndTime <- obj .:? "meetingEndTime"
    return $ getTimeSlots meetingDay meetingStartTime meetingEndTime
parseTimes _ = return []


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

-- | Takes a day and start/end times then generates a set of 30-minute timeslots
getTimeSlots :: Maybe String -> Maybe String -> Maybe String -> [Time]
getTimeSlots (Just day) (Just start) (Just end) = do
    let dayDbl = getDayVal day
        startDbl = getHourVal start
        endDbl = getHourVal end
    [Time [dayDbl, timeDbl] | timeDbl <- [startDbl, (startDbl + 0.5) .. (endDbl - 0.5)]]
getTimeSlots _ _ _ = []
