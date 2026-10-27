{-# LANGUAGE DeriveGeneric #-}

module Models.Time (
    TimeData (..),
    buildTime,
) where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM)
import Database.Tables (Building, Time (..))
import qualified Database.Tables as Tables
import GHC.Generics (Generic)
import Models.Building (getBuilding)

-- | The time and location data for a single meeting occurrence, as returned by the back-end to the front-end.
--
-- This is different from the schema-defined 'Time' type (in "Database.Tables").
-- Whereas 'Time' stores a raw room code string, 'TimeData' resolves the location to a
-- full 'Building' object for JSON serialization to the client.
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

-- | Convert a Time record into a TimeData by resolving room codes to Buildings
buildTime :: Time -> SqlPersistM TimeData
buildTime t = do
    location <- getBuilding (Tables.timeLocation t)
    return $
        TimeData
            (Tables.timeSession t)
            (Tables.timeWeekDay t)
            (Tables.timeStartHour t)
            (Tables.timeEndHour t)
            location
