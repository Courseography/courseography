module WebParsing.UtsgJsonParser
     (getAllCourses,
      insertAllMeetings) where

import Config (databasePath)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), decodeFileStrict, (.!=), (.:?))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite (SqlPersistM, insert, insertMany_, runSqlite, selectFirst, (==.))
import Database.Tables (Courses (..), EntityField (CoursesCode), MeetTime (..), Meeting (..),
                        Times (..), buildTimes)

coursesJson :: FilePath
coursesJson = "courses.json"

-- | Parse all timetable data.
getAllCourses :: IO ()
getAllCourses = do
    runSqlite databasePath insertAllMeetings

-- | Retrieve and store all timetable data for the given department.
insertAllMeetings :: SqlPersistM ()
insertAllMeetings = do
    deleteWhere ([] :: [Filter Times])
    deleteWhere ([] :: [Filter Meeting])
    liftIO . print $ T.pack "parsing JSON data"
    resp <- liftIO $ decodeFileStrict coursesJson
    let coursesLst :: Maybe (HM.HashMap T.Text (Maybe DB)) = resp
        courseData = maybe [] (map dbData . catMaybes . HM.elems) coursesLst
        -- courseData contains courses and sections;
        -- only sections are currently stored here.
        (_, sections) = unzip courseData
        meetings = concat sections
    mapM_ insertMeeting meetings

-- | Insert a meeting and its corresponding Times into the database.
insertMeeting :: MeetTime -> SqlPersistM ()
insertMeeting (MeetTime meetingData meetingTime) = do
    -- Check that the meeting belongs to a course that exists
    let code = meetingCode meetingData
    courseKey <- selectFirst [ CoursesCode ==. code ] []
    case courseKey of
        Just _ -> do
          meetingKey <- insert meetingData
          let allTimes = map (buildTimes meetingKey) meetingTime
          insertMany_ allTimes
        Nothing -> return ()

newtype DB = DB { dbData :: (Courses, [MeetTime]) }
  deriving Show

instance FromJSON DB where
    parseJSON (Object o) = do
      course <- parseJSON (Object o)
      session :: T.Text <- o .:? "section" .!= "F"
      meetingTimesMap :: HM.HashMap T.Text MeetTime <- o .:? "meetings" .!= HM.empty
      let allMeetingsTimes = map (\m -> m {meetInfo = (meetInfo m) { meetingCode = coursesCode course, meetingSession = session}}) (HM.elems meetingTimesMap)
      return $ DB (course, allMeetingsTimes)
    parseJSON _ = fail "Invalid section"
