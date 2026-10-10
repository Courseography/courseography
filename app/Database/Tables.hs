{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
--     Module      : Database.Tables
--     Description : The database schema (and some helpers).
--
-- This module defines the database schema. It uses Template Haskell to also
-- create new types for these values so that they can be used in the rest of
-- the application.
--
-- Though types and typeclass instances are created automatically, we currently
-- have a few manually-generated spots to clean up. This should be rather
-- straightforward.
module Database.Tables where

import Data.Aeson (
    FromJSON (parseJSON),
    ToJSON (toJSON),
    genericToJSON,
    withObject,
    (.!=),
    (.:?),
 )
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

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|

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
    prep T.Text Maybe
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
    deriving Show Eq

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

instance ToJSON Program
instance ToJSON Building

instance ToJSON Meeting where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier =
                    lowerFirst
                        . drop 7
                }
      where
        lowerFirst :: [Char] -> String
        lowerFirst [] = ""
        lowerFirst (fieldHead : fieldTail) = toLower fieldHead : fieldTail

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
      where
        parseInstr :: Value -> Parser T.Text
        parseInstr (Object io) = do
            firstName <- io .:? "firstName" .!= ""
            lastName <- io .:? "lastName" .!= ""
            return (T.concat [firstName, " ", lastName])
        parseInstr _ = return ""
