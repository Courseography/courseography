module Models.Time
    (buildTime,
    buildTimes) where
import Database.Persist.Sqlite (SqlPersistM)
import Database.Tables (MeetingId, Time (..), Time' (..), Times (..))
import Models.Building (getBuilding)

-- | Convert a Times record into a Time by resolving room codes to Buildings
buildTime :: Times -> SqlPersistM Time
buildTime t = do
    room1 <- getBuilding (timesFirstRoom t)
    room2 <- getBuilding (timesSecondRoom t)
    return $ Time (timesWeekDay t)
        (timesStartHour t)
        (timesEndHour t)
        room1
        room2

buildTimes :: MeetingId -> Time' -> Times
buildTimes meetingKey t =
    Times (weekDay' t)
        (startHour' t)
        (endHour' t)
        meetingKey
        (firstLocation' t)
        (secondLocation' t)
