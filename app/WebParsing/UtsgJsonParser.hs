module WebParsing.UtsgJsonParser
     (parseTimetable,
      parseAllCourses,
      insertAllMeetings) where

import Config (databasePath, orgApiUrl, timetableApiUrl)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), decode, decodeFileStrict, (.!=), (.:?))
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap as KM hiding (insert, map)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.Persist.Sqlite (Filter, SqlPersistM, Update, entityKey, deleteWhere, upsert,
                                insertMany_, runSqlite, selectFirst, (==.), (=.))
import Database.Tables (Courses (..), EntityField (..), MeetTime (..), Meeting (..),
                        Times (..), buildTimes)
import Network.HTTP.Conduit (simpleHttp)

coursesJson :: FilePath
coursesJson = "courses.json"

parseAllCourses :: IO ()
parseAllCourses = do
    runSqlite databasePath parseCourses

parseCourses :: SqlPersistM ()
parseCourses = do
    deleteWhere ([] :: [Filter Times])
    deleteWhere ([] :: [Filter Meeting])
    liftIO . print $ T.pack "parsing JSON data"
    resp <- liftIO $ decodeFileStrict coursesJson
    let coursesLst :: Maybe (HM.HashMap T.Text (Maybe DB)) = resp
        courseData = maybe [] (map dbData . catMaybes . HM.elems) coursesLst
        (_, sections) = unzip courseData
        meetings = concat sections
    mapM_ insertMeeting meetings

-- | Parse all timetable data.
parseTimetable :: IO ()
parseTimetable = do
    orgs <- getOrgs
    runSqlite databasePath $ mapM_ insertAllMeetings orgs

-- | Return a list of all the "orgs" in FAS. These are the values which can be
--   passed to the timetable API with the "org" key.
getOrgs :: IO [T.Text]
getOrgs = do
    resp <- simpleHttp orgApiUrl
    let rawJSON :: Maybe (HM.HashMap T.Text Object) = decode resp
    return $ maybe [] (concatMap $ map toText . KM.keys) rawJSON

-- | Retrieve and store all timetable data for the given department.
insertAllMeetings :: T.Text -> SqlPersistM ()
insertAllMeetings org = do
    liftIO . print $ T.append "parsing JSON data from: " org
    resp <- liftIO . simpleHttp $ T.unpack (T.append timetableApiUrl org)
    let coursesLst :: Maybe (HM.HashMap T.Text (Maybe DB)) = decode resp
        courseData = maybe [] (map dbData . catMaybes . HM.elems) coursesLst
        -- courseData contains courses and sections;
        -- only sections are currently stored here.
        (_, sections) = unzip courseData
        meetings = concat sections
    mapM_ insertMeeting meetings

-- | Insert or update a meeting and then delete
--   and re-insert the corresponding Times into the database.
insertMeeting :: MeetTime -> SqlPersistM ()
insertMeeting (MeetTime meetingData meetingTime) = do
    -- Check that the meeting belongs to a course that exists
    let code = meetingCode meetingData
    courseKey <- selectFirst [ CoursesCode ==. code ] []
    case courseKey of
        Just _ -> do
          entity <- upsert meetingData (meetingUpdates meetingData)
          let meetingKey = entityKey entity
          deleteWhere [ TimesMeeting ==. meetingKey ]
          let allTimes = map (buildTimes meetingKey) meetingTime
          insertMany_ allTimes
        Nothing -> return ()

-- | Update the entries of the Meeting Table if necessary
meetingUpdates :: Meeting -> [Update Meeting]
meetingUpdates m = [ MeetingCode =. meetingCode m
                   , MeetingSession =. meetingSession m
                   , MeetingSection =. meetingSection m
                   , MeetingCap =. meetingCap m
                   , MeetingInstructor =. meetingInstructor m
                   , MeetingEnrol =. meetingEnrol m
                   , MeetingWait =. meetingWait m
                   , MeetingExtra =. meetingExtra m
                   ]

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
