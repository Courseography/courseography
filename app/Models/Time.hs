{-# LANGUAGE DeriveGeneric #-}

module Models.Time (
    TimeData (..),
    buildTime,
) where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM)
import Database.Tables (Building, Times (..))
import GHC.Generics (Generic)
import Models.Building (getBuilding)

-- | The time and location data for a single meeting occurrence, as returned by the back-end to the front-end.
--
-- This is different from the schema-defined 'Times' type (in "Database.Tables").
-- Whereas 'Times' stores a raw room code string, 'TimeData' resolves the location to a
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
