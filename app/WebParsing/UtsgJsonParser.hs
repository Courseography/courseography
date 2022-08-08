module WebParsing.UtsgJsonParser
     (getAllCourses,
      -- getOrgs,
      insertAllMeetings) where

import Config (databasePath, orgApiUrl, timetableApiUrl)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), decode, decodeFileStrict, (.!=), (.:?))
import Data.Aeson.Key (toText)
import Data.Aeson.KeyMap as KM hiding (insert, map)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert, insertMany_, runSqlite, selectFirst, (==.))
import Database.Tables (Courses (..), EntityField (CoursesCode), MeetTime (..), Meeting (..),
                        buildTimes)
import Network.HTTP.Conduit (simpleHttp)

coursesJson :: FilePath
coursesJson = "courses.json"

{-
getCourses :: IO B.ByteString
getCourses = B.readFile coursesJson
-}

-- | Parse all timetable data.
getAllCourses :: IO ()
getAllCourses = do
    -- orgs <- getOrgs
    runSqlite databasePath insertAllMeetings -- $ mapM_ insertAllMeetings orgs

{-
-- | Return a list of all the "orgs" in FAS. These are the values which can be
--   passed to the timetable API with the "org" key.
getOrgs :: IO [T.Text]
getOrgs = do
    resp <- simpleHttp orgApiUrl
    let rawJSON :: Maybe (HM.HashMap T.Text Object) = decode resp
    return $ maybe [] (concatMap $ map toText . KM.keys) rawJSON
-}

-- | Retrieve and store all timetable data for the given department.
insertAllMeetings :: {- T.Text -> -} SqlPersistM ()
insertAllMeetings {- org -} = do
    liftIO . print "parsing JSON data"
    -- resp <- liftIO . simpleHttp $ T.unpack (T.append timetableApiUrl org) -- where we are getting a network connection error
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
