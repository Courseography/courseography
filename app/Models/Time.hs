module Models.Time (
    buildTime,
    buildTimes,
) where

import Database.Persist.Sqlite (SqlPersistM)
import Database.Tables (MeetingId, Time (..), Time' (..), Times (..))
import Models.Building (getBuilding)

-- | Convert a Times record into a Time by resolving room codes to Buildings
buildTime :: Times -> SqlPersistM Time
buildTime t = do
    location <- getBuilding (timesLocation t)
    return $
        Time
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
