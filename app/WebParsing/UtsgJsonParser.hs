module WebParsing.UtsgJsonParser
     (getAllCourses,
      getOrgs,
      insertAllCourses) where

import Data.Aeson ((.:?), (.!=), decode, FromJSON(parseJSON), Value(..), Object, withObject)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Conduit (simpleHttp)
import Config (databasePath)
import Database.Tables (Courses(..), EntityField(CoursesCode), Meeting(..), EntityField(MeetingCode), EntityField(MeetingSection), EntityField(MeetingSession), Times(..))
import Database.Persist.Sqlite (runSqlite, insert_, SqlPersistM, selectKeysList, (==.))
import Database.Persist.Class (Key)
--import Text.Parsec.Text (Parser)


-- | URLs for the Faculty of Arts and Science API
timetableURL :: T.Text
timetableURL = "https://timetable.iit.artsci.utoronto.ca/api/20179/courses?code="

orgURL :: String
orgURL = "https://timetable.iit.artsci.utoronto.ca/api/orgs"

-- | Parse all timetable data.
getAllCourses :: IO ()
getAllCourses = do
    orgs <- getOrgs
    runSqlite databasePath $ mapM_ insertAllCourses orgs

-- | Return a list of all the "orgs" in FAS. These are the values which can be
--   passed to the timetable API with the "org" key.
getOrgs :: IO [T.Text]
getOrgs = do
    resp <- simpleHttp orgURL
    let rawJSON :: Maybe (HM.HashMap T.Text Object) = decode resp
    return $ maybe [] (concatMap HM.keys . HM.elems) rawJSON

-- | Retrieve and store all timetable data for the given department.
insertAllCourses :: T.Text -> SqlPersistM ()
insertAllCourses org = do
    liftIO . print $ T.append "parsing JSON data from: " org
    resp <- liftIO . simpleHttp $ T.unpack (T.append timetableURL org)
    let coursesLst :: Maybe (HM.HashMap T.Text (Maybe DB)) = decode resp
        courseData = maybe [] (map dbData . catMaybes . HM.elems) coursesLst
        -- courseData contains courses and sections;
        -- only sections are currently stored here.
        (_, sections) = unzip courseData
        meetings = concat sections
    mapM_ insertMeeting meetings

getCourseKey :: T.Text -> SqlPersistM (Maybe (Key Courses))
getCourseKey code = do
    keyListCourse :: [Key Courses] <- selectKeysList [ CoursesCode ==. code ] []
    return $ case keyListCourse of
        [] -> Nothing
        _ -> Just (head keyListCourse)


getMeetingKey :: T.Text -> T.Text -> T.Text -> SqlPersistM (Maybe (Key Meeting))
getMeetingKey meetCode meetSession meetSection = do
  keyListMeeting :: [Key Meeting] <- selectKeysList [ (MeetingCode ==. meetCode), (MeetingSection ==. meetSection), (MeetingSession ==. meetSession)] []
  return $ case keyListMeeting of
    [] -> Nothing
    _ -> Just (head keyListMeeting)

insertMeeting :: MeetingTimes -> SqlPersistM ()
insertMeeting meet = do
    let meetingInfo = meetingData meet
    courseKey <- getCourseKey (meetingCode meetingInfo)
    case courseKey of
        Just _ -> do
          insert_ meetingInfo
          meetingKey <- getMeetingKey (meetingCode meetingInfo) (meetingSession meetingInfo) (meetingSection meetingInfo)
          case meetingKey of
              Just _ -> do
                mapM_ (\t -> insert_ $ t {timesMeeting = meetingKey}) $ times meet
              Nothing -> return ()
        Nothing -> return ()


newtype DB = DB { dbData :: (Courses, [MeetingTimes]) }
  deriving Show

-- parseMeetingTimes :: Value -> Parser (Meeting, [AllTimes])
-- parseMeetingTimes (Object obj) = do
--     meetingData <- parseJSON(Object obj)
--     timeMap :: HM.HashMap T.Text AllTimes <- o .:? "schedule" .!= HM.empty
--     return (meetingData, timeMap)
-- parseMeetingTimes _ = return (Nothing, [])

parseMeetingTimes:: (T.Text, T.Text, Meeting, [Times]) -> MeetingTimes
parseMeetingTimes (code, session, meetTimes, allTimes) =
    MeetingTimes {meetingData = meetTimes {meetingCode = code, meetingSession = session}, times = allTimes }

instance FromJSON DB where
    parseJSON (Object o) = do
      course <- parseJSON (Object o)
      session :: T.Text <- o .:? "section" .!= "F"
      meetingTimesMap :: HM.HashMap T.Text MeetingTimes <- o .:? "meetings" .!= HM.empty
      let allMeetingsTimes = map (\meetTime -> parseMeetingTimes ((coursesCode course), session, (meetingData meetTime), (times meetTime))) (HM.elems meetingTimesMap)
          -- Fix manualTutorialEnrolment and manualPracticalEnrolment
          manTut = any (T.isPrefixOf "TUT" . meetingSection) $ map (\m -> meetingData m) allMeetingsTimes
          manPra = any (T.isPrefixOf "PRA" . meetingSection) $ map (\m -> meetingData m) allMeetingsTimes
      return $ DB (course { coursesManualTutorialEnrolment = Just manTut,
                            coursesManualPracticalEnrolment = Just manPra },
                  allMeetingsTimes)
      -- where
      --     setCode code session m =
      --       MeetingTimes {meetingData = meetingData m {meetingCode = code, meetingSession = session}, times=times m}
    parseJSON _ = fail "Invalid section"

-- newtype Meetings = Meetings { meeting :: Meeting }
--   deriving Show

-- instance FromJSON Meetings where
--   parseJSON x = fmap Meetings (parseJSON x)

newtype AllTimes = AllTimes { timesData :: Times }
  deriving Show

instance FromJSON MeetingTimes where
  parseJSON = withObject "meetingtimes" $ \o -> do
    meeting <- parseJSON (Object o)
    timeMap :: HM.HashMap T.Text AllTimes <- o .:? "schedule" .!= HM.empty
    let allTimes = map (toTime timesData) (HM.elems timeMap)
    return $ MeetingTimes meeting allTimes
    where
      toTime time =
        time

data MeetingTimes = MeetingTimes { meetingData :: Meeting, times :: [Times] }
  deriving Show

instance FromJSON AllTimes where
  parseJSON x = fmap AllTimes (parseJSON x)
