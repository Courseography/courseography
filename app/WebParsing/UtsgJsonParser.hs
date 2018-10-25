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
import Database.Tables (Courses(..), EntityField(CoursesCode), Meeting(..), EntityField(MeetingCode), EntityField(MeetingSection), EntityField(MeetingSession), Times(..), MeetTimes(..))
import Database.Persist.Sqlite (runSqlite, insert_, SqlPersistM, selectKeysList, (==.))
import Database.Persist.Class (Key)
-- import Control.Applicative ((<|>))


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

-- | Retrieve the key for the course with the given course code.
getCourseKey :: T.Text -> SqlPersistM (Maybe (Key Courses))
getCourseKey code = do
    keyListCourse :: [Key Courses] <- selectKeysList [ CoursesCode ==. code ] []
    return $ case keyListCourse of
        [] -> Nothing
        _ -> Just (head keyListCourse)

-- | Retrieve the key for the meeting with the given meeting code, session and section.
getMeetingKey :: T.Text -> T.Text -> T.Text -> SqlPersistM (Maybe (Key Meeting))
getMeetingKey meetCode meetSession meetSection = do
  keyListMeeting :: [Key Meeting] <- selectKeysList [ (MeetingCode ==. meetCode), (MeetingSection ==. meetSection), (MeetingSession ==. meetSession)] []
  return $ case keyListMeeting of
    [] -> Nothing
    _ -> Just (head keyListMeeting)

-- | Store a meeting's data and times.
insertMeeting :: MeetTimes -> SqlPersistM ()
insertMeeting meet = do
    -- Check that the meeting belongs to a course that exists
    courseKey <- getCourseKey (meetingCode $ meetingData meet)
    case courseKey of
        Just _ -> do
          insert_ $ meetingData meet
          meetingKey <- getMeetingKey (meetingCode $ meetingData meet) (meetingSession $ meetingData meet) (meetingSection $ meetingData meet)
          case meetingKey of
              Just _ -> mapM_ (\t -> insert_ $ t {timesMeeting = meetingKey}) $ timesData meet
              Nothing -> return ()
        Nothing -> return ()

parseMeetingTimes:: T.Text -> T.Text -> Meeting -> [Times] -> MeetTimes
parseMeetingTimes code session meetTimes allTimes =
    MeetTimes {meetingData = meetTimes {meetingCode = code, meetingSession = session}, timesData = allTimes }

newtype DB = DB { dbData :: (Courses, [MeetTimes]) }
  deriving Show

instance FromJSON DB where
    parseJSON (Object o) = do
      course <- parseJSON (Object o)
      session :: T.Text <- o .:? "section" .!= "F"
      meetingTimesMap :: HM.HashMap T.Text MeetTimes <- o .:? "meetings" .!= HM.empty
      let allMeetingsTimes = map (\meetTime -> parseMeetingTimes (coursesCode course) session (meetingData meetTime) (timesData meetTime)) (HM.elems meetingTimesMap)
          -- Fix manualTutorialEnrolment and manualPracticalEnrolment
          manTut = any (T.isPrefixOf "TUT" . meetingSection) $ map meetingData allMeetingsTimes
          manPra = any (T.isPrefixOf "PRA" . meetingSection) $ map meetingData allMeetingsTimes
      return $ DB (course { coursesManualTutorialEnrolment = Just manTut,
                            coursesManualPracticalEnrolment = Just manPra },
                  allMeetingsTimes)
    parseJSON _ = fail "Invalid section"
