{-# LANGUAGE EmptyDataDecls,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             OverloadedStrings,
             DeriveGeneric,
             QuasiQuotes,
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
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import GHC.Generics

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

Courses json
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

Lecture json
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
    deriving Show

Tutorial json
    code T.Text
    section T.Text Maybe
    session T.Text
    times [Time]
    deriving Show

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

data CourseInfo = CourseInfo {code :: String, section :: String, session :: String, time :: [Time]} deriving (Show)

instance ToJSON Course
instance ToJSON Session
instance ToJSON Time

-- instance FromJSON required so that tables can be parsed into JSON,
-- not necessary otherwise.
instance FromJSON Time
instance FromJSON SvgJSON

-- | Converts a Double to a T.Text.
-- This removes the period from the double, as the JavaScript code,
-- uses the output in an element's ID, which is then later used in
-- jQuery. @.@ is a jQuery meta-character, and must be removed from the ID.
convertTimeToString :: Time -> [T.Text]
convertTimeToString (Time [day, timeNum]) =
  [T.pack . show . floor $ day,
   T.replace "." "-" . T.pack . show $ timeNum]
