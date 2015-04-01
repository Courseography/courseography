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

module Database.Tables where

import Database.Persist.TH
import Database.DataType
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Data.Int
import Control.Monad
import Control.Applicative

data Time = Time { timeField :: [Double] } deriving (Show, Read, Eq)
derivePersistField "Time"

type Point = (Double, Double)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Courses json
    code T.Text
    title T.Text Maybe
    description T.Text Maybe
    manualTutorialEnrolment Bool Maybe
    manualPracticalEnrolment Bool Maybe 
    prereqs T.Text Maybe
    exclusions T.Text Maybe
    breadth T.Text Maybe
    distribution T.Text Maybe
    prereqString T.Text Maybe
    deriving Show

Lectures
    code T.Text
    session T.Text
    section T.Text
    times [Time]
    capacity Int
    instructor T.Text
    enrolled Int
    waitlist Int
    extra Int
    timeStr T.Text
    deriving Show

Tutorials
    code T.Text
    section T.Text Maybe
    session T.Text
    times [Time]
    timeStr T.Text
    deriving Show

Breadth
    bId Int
    description String
    deriving Show

Distribution
    dId Int
    description String
    deriving Show

Graph json
    gId Int64
    title String
    deriving Show

Text
    gId Int64
    rId String
    pos Point
    text String
    deriving Show

Shape
    gId Int64
    id_ String
    pos Point
    width Double
    height Double
    fill String
    stroke String
    text [Text]
    tolerance Double
    type_ ShapeType

Path
    gId Int64
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
|]

-- | A Lecture.
data Lecture =
    Lecture { extra :: Int,
              section :: T.Text,
              cap :: Int,
              time_str :: T.Text,
              time :: [[Double]],
              instructor :: T.Text,
              enrol :: Maybe Int,
              wait :: Maybe Int
            } deriving Show

-- | A Tutorial.
data Tutorial =
    Tutorial { tutorialSection :: Maybe T.Text,
               times :: [[Double]],
               timeStr :: T.Text
             } deriving Show

-- | A Session.
data Session =
    Session { lectures :: [Lecture],
              tutorials :: [Tutorial]
            } deriving Show

-- | A Course.
-- each element of prereqs can be one of three things:
--    * a one-element list containing a course code
--    * a list starting with "and", and 2 or more course codes
--    * a list starting with "or", and 2 or more course codes
data Course =
    Course { breadth :: Maybe T.Text,
             description :: Maybe T.Text,
             title :: Maybe T.Text,
             prereqString :: Maybe T.Text,
             f :: Maybe Session,
             s :: Maybe Session,
             y :: Maybe Session,
             name :: !T.Text,
             exclusions :: Maybe T.Text,
             manualTutorialEnrol :: Maybe Bool,
             manualPracticalEnrol :: Maybe Bool, 
             distribution :: Maybe T.Text,
             prereqs :: Maybe T.Text
           } deriving Show

instance ToJSON Course where
  toJSON (Course breadth description title prereqString f s y name exclusions manualTutorialEnrol manualPracticalEnrol distribution prereqs)
          = object ["breadth" .= breadth,
                    "description" .= description,
                    "title" .= title,
                    "prereqString" .= prereqString,
                    "F" .= f,
                    "S" .= s,
                    "Y" .= y,
                    "name" .= name,
                    "exclusions" .= exclusions,
                    "manualTutorialEnrolment" .= manualTutorialEnrol,
                    "manualPracticalEnrolment" .= manualPracticalEnrol,
                    "distribution" .= distribution,
                    "prereqs" .= prereqs
                   ]

instance ToJSON Session where
  toJSON (Session lectures tutorials)
          = object ["lectures" .= lectures,
                    "tutorials" .= tutorials
                   ]

instance ToJSON Lecture where
  toJSON (Lecture extra section cap time_str time instructor enrol wait)
          = object ["extra" .= extra,
                    "section" .= section,
                    "cap" .= cap,
                    "time_str" .= time_str,
                    "time" .= map convertTimeToString time,
                    "instructor" .= instructor,
                    "enrol" .= enrol,
                    "wait" .= wait
                   ]

instance ToJSON Tutorial where
  toJSON (Tutorial Nothing times timeStr) =
      Array $ V.fromList [toJSON (map convertTimeToString times), toJSON timeStr]
  toJSON (Tutorial (Just value) times timeStr) =
      Array $ V.fromList [toJSON value, toJSON (map convertTimeToString times), toJSON timeStr]

-- | Converts a Double to a T.Text.
-- This removes the period from the double, as the JavaScript code,
-- uses the output in an element's ID, which is then later used in
-- jQuery. `.` is a jQuery meta-character, and must be removed from the ID.
convertTimeToString :: [Double] -> [T.Text]
convertTimeToString [day, time] =
  [T.pack . show . floor $ day,
   T.replace "." "-" . T.pack . show $ time]
