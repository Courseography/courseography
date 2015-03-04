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
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Control.Monad
import Control.Applicative

data Time = Time { timeField :: [Int] } deriving (Show, Read, Eq)
derivePersistField "Time"


data Point = Point { point :: (Rational, Rational) } deriving (Show, Read, Eq)
derivePersistField "Point"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Courses json
    code T.Text
    title T.Text Maybe
    description T.Text Maybe
    manualTutorialEnrolment Bool Maybe
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

Graphs
    gId Int
    title String
    deriving Show

Rects
    gId Int
    rId String
    width Rational
    height Rational
    xPos Rational
    yPos Rational
    fill String
    stroke String
    isHybrid Bool
    deriving Show

Texts
    gId Int
    rId String
    xPos Rational
    yPos Rational
    text String
    deriving Show

Paths
    d [Point]
    fill String
    stroke String
    isRegion Bool
    deriving Show

Ellipses
    xPos Rational
    yPos Rational
    rx Rational
    ry Rational
    stroke String
|]

-- | A Lecture.
data Lecture =
    Lecture { extra :: Int,
              section :: T.Text,
              cap :: Int,
              time_str :: T.Text,
              time :: [[Int]],
              instructor :: T.Text,
              enrol :: Maybe Int,
              wait :: Maybe Int
            } deriving Show

-- | A Tutorial.
data Tutorial =
    Tutorial { tutorialSection :: Maybe T.Text,
               times :: [[Int]],
               timeStr :: T.Text
             } deriving Show

-- | A Session.
data Session =
    Session { lectures :: [Lecture],
              tutorials :: [Tutorial]
            } deriving Show

-- | A Course.
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
             distribution :: Maybe T.Text,
             prereqs :: Maybe Array
           } deriving Show

instance FromJSON Course where
    parseJSON (Object v) =
        Course <$> v .:? "breadth"
               <*> v .:? "description"
               <*> v .:? "title"
               <*> v .:? "prereqString"
               <*> v .:? "F"
               <*> v .:? "S"
               <*> v .:? "Y"
               <*> v .:  "name"
               <*> v .:? "exclusions"
               <*> v .:? "manualTutorialEnrolment"
               <*> v .:? "distribution"
               <*> v .:? "prereqs"
    parseJSON _ = mzero

instance ToJSON Course where
  toJSON (Course breadth description title prereqString f s y name exclusions manualTutorialEnrol distribution prereqs)
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
                    "distribution" .= distribution,
                    "prereqs" .= prereqs
                   ]

instance FromJSON Session where
    parseJSON (Object v) =
        Session <$> v .: "lectures"
                <*> v .: "tutorials"
    parseJSON _ = mzero

instance ToJSON Session where
  toJSON (Session lectures tutorials)
          = object ["lectures" .= lectures,
                    "tutorials" .= tutorials
                   ]

instance FromJSON Lecture where
    parseJSON (Object v) =
        Lecture <$> v .:  "extra"
                <*> v .:  "section"
                <*> v .:  "cap"
                <*> v .:  "time_str"
                <*> v .:  "time"
                <*> v .:  "instructor"
                <*> v .:? "enrol"
                <*> v .:? "wait"
    parseJSON _ = mzero

instance ToJSON Lecture where
  toJSON (Lecture extra section cap time_str time instructor enrol wait)
          = object ["extra" .= extra,
                    "section" .= section,
                    "cap" .= cap,
                    "time_str" .= time_str,
                    "time" .= time,
                    "instructor" .= instructor,
                    "enrol" .= enrol,
                    "wait" .= wait
                   ]

instance FromJSON Tutorial where
    parseJSON (Array v)
        | V.length v == 2 = do
            times <- parseJSON $ v V.! 0
            timeStr <- parseJSON $ v V.! 1
            return $ Tutorial Nothing times timeStr
        | V.length v == 3 = do
            tutorialSection <- parseJSON $ v V.! 0
            times <- parseJSON $ v V.! 1
            timeStr <- parseJSON $ v V.! 2
            return $ Tutorial tutorialSection times timeStr
        | otherwise = mzero
    parseJSON _ = mzero

instance ToJSON Tutorial where
  toJSON (Tutorial Nothing times timeStr) =
      Array $ V.fromList [toJSON times, toJSON timeStr]
  toJSON (Tutorial (Just value) times timeStr) =
      Array $ V.fromList [toJSON value, toJSON times, toJSON timeStr]
