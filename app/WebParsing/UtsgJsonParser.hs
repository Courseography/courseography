module WebParsing.UtsgJsonParser
     (getAllCourses,
      getOrgs,
      insertAllMeetings) where

import Data.Aeson ((.:?), (.!=), decode, FromJSON(parseJSON), Value(..), Object)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Conduit (simpleHttp)
import Config (databasePath)
import Database.Tables (Courses(..), EntityField(CoursesCode), Meeting(..), MeetTime(..), buildTimes)
import Database.Persist.Sqlite (runSqlite, insert_, SqlPersistM, (==.), insert, selectFirst)

-- | URLs for the Faculty of Arts and Science API
timetableURL :: T.Text
timetableURL = "https://timetable.iit.artsci.utoronto.ca/api/20189/courses?org="

orgURL :: String
orgURL = "https://timetable.iit.artsci.utoronto.ca/api/orgs"

-- | Parse all timetable data.
getAllCourses :: IO ()
getAllCourses = do
    orgs <- getOrgs
    runSqlite databasePath $ mapM_ insertAllMeetings orgs

-- | Return a list of all the "orgs" in FAS. These are the values which can be
--   passed to the timetable API with the "org" key.
getOrgs :: IO [T.Text]
getOrgs = do
    resp <- simpleHttp orgURL
    let rawJSON :: Maybe (HM.HashMap T.Text Object) = decode resp
    return $ maybe [] (concatMap HM.keys . HM.elems) rawJSON

-- | Retrieve and store all timetable data for the given department.
insertAllMeetings :: T.Text -> SqlPersistM ()
insertAllMeetings org = do
    liftIO . print $ T.append "parsing JSON data from: " org
    resp <- liftIO . simpleHttp $ T.unpack (T.append timetableURL org)
    let coursesLst :: Maybe (HM.HashMap T.Text (Maybe DB)) = decode resp
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
          mapM_ insert_ allTimes
        Nothing -> return ()

newtype DB = DB { dbData :: (Courses, [MeetTime]) }
  deriving Show

instance FromJSON DB where
    parseJSON (Object o) = do
      course <- parseJSON (Object o)
      session :: T.Text <- o .:? "section" .!= "F"
      meetingTimesMap :: HM.HashMap T.Text MeetTime <- o .:? "meetings" .!= HM.empty
      let allMeetingsTimes = map (\m -> m {meetData = (meetData m) { meetingCode = (coursesCode course), meetingSession = session}}) (HM.elems meetingTimesMap)
          -- Fix manualTutorialEnrolment and manualPracticalEnrolment
          manTut = any (T.isPrefixOf "TUT" . meetingSection) $ map meetData allMeetingsTimes
          manPra = any (T.isPrefixOf "PRA" . meetingSection) $ map meetData allMeetingsTimes
      return $ DB (course { coursesManualTutorialEnrolment = Just manTut,
                            coursesManualPracticalEnrolment = Just manPra },
                  allMeetingsTimes)
    parseJSON _ = fail "Invalid section"
