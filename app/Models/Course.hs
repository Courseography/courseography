module Models.Course
    (returnCourse, queryCourse) where

import Config (runDb)
import Data.Aeson (Value, toJSON)
import qualified Data.Text as T (Text, append, take, toUpper)
import Database.CourseQueries (buildCourse, buildMeetTimes)
import Database.Persist.Sqlite (Entity, SqlPersistM, entityVal, selectFirst, selectList, (<-.))
import Database.Tables as Tables

-- | Queries the database for all matching lectures, tutorials,
meetingQuery :: [T.Text] -> SqlPersistM [MeetTime']
meetingQuery meetingCodes = do
    allMeetings <- selectList [MeetingCode <-. map (T.take 6) meetingCodes] []
    mapM buildMeetTimes allMeetings

-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO (Maybe Course)
returnCourse lowerStr = runDb $ do
    let courseStr = T.toUpper lowerStr
    -- TODO: require the client to pass the full course code
    let fullCodes = [courseStr, T.append courseStr "H1", T.append courseStr "Y1"]
    sqlCourse :: (Maybe (Entity Courses)) <- selectFirst [CoursesCode <-. fullCodes] []
    case sqlCourse of
      Nothing -> return Nothing
      Just course -> do
        meetings <- meetingQuery fullCodes
        Just <$> buildCourse meetings
                                (entityVal course)

-- | Queries the database for all information about @course@, and returns a JSON
-- object representing the course.
queryCourse :: T.Text -> IO Value
queryCourse str = do
    courseJSON <- returnCourse str
    return $ toJSON courseJSON
