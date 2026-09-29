{-# LANGUAGE DeriveGeneric #-}

module Models.Time (
    TimeData (..),
    buildTime,
    buildTimes,
) where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM)
import Database.Tables (Building, MeetingId, Time' (..), Times (..))
import GHC.Generics (Generic)
import Models.Building (getBuilding)

data TimeData
    = TimeData
    { timeSession :: Maybe T.Text
    , weekDay :: Double
    , startHour :: Double
    , endHour :: Double
    , timeLocation :: Maybe Building
    }
    deriving (Show, Generic)

instance ToJSON TimeData

-- | Convert a Times record into a TimeData by resolving room codes to Buildings
buildTime :: Times -> SqlPersistM TimeData
buildTime t = do
    location <- getBuilding (timesLocation t)
    return $
        TimeData
            (timesSession t)
            (timesWeekDay t)
            (timesStartHour t)
            (timesEndHour t)
            location

buildTimes :: MeetingId -> Time' -> Times
buildTimes meetingKey t =
    Times
        (timeSession' t)
        (weekDay' t)
        (startHour' t)
        (endHour' t)
        meetingKey
        (timeLocation' t)
