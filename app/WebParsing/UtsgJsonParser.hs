module WebParsing.UtsgJsonParser
     (getAllCourses,
      getOrgs,
      insertAllCourses) where

import Data.Aeson ((.:?), (.!=), decode, FromJSON(parseJSON), Value(..), Object)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Conduit (simpleHttp)
import Config (databasePath)
import Database.Tables (Courses(..), EntityField(CoursesCode), Meeting(..))
import Database.Persist.Sqlite (runSqlite, insert_, SqlPersistM, selectKeysList, (==.))
import Database.Persist.Class (Key)


-- | URLs for the Faculty of Arts and Science API
timetableURL :: T.Text
timetableURL = "https://timetable.iit.artsci.utoronto.ca/api/20189/courses?org="

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
    mapM_ insertCourse meetings

getCourseKey :: T.Text -> SqlPersistM (Maybe (Key Courses))
getCourseKey code = do
    keyListCourse :: [Key Courses] <- selectKeysList [ CoursesCode ==. code ] []
    return $ case keyListCourse of
        [] -> Nothing
        _ -> Just (head keyListCourse)

insertCourse :: Meeting -> SqlPersistM ()
insertCourse meet = do
    courseKey <- getCourseKey (meetingCode meet)
    case courseKey of
        Just _ -> insert_ meet
        Nothing -> return ()

newtype DB = DB { dbData :: (Courses, [Meeting]) }
  deriving Show

instance FromJSON DB where
    parseJSON (Object o) = do
      course <- parseJSON (Object o)
      session :: T.Text <- o .:? "section" .!= "F"
      meetingMap :: HM.HashMap T.Text Meetings <- o .:? "meetings" .!= HM.empty
      let meetings = map (setCode (coursesCode course) session . meeting) (HM.elems meetingMap)
          -- Fix manualTutorialEnrolment and manualPracticalEnrolment
          manTut = any (T.isPrefixOf "TUT" . meetingSection) meetings
          manPra = any (T.isPrefixOf "PRA" . meetingSection) meetings
      return $ DB (course { coursesManualTutorialEnrolment = Just manTut,
                            coursesManualPracticalEnrolment = Just manPra },
                   meetings)
      where
          setCode code session m =
            m {meetingCode = code, meetingSession = session}
    parseJSON _ = fail "Invalid section"

newtype Meetings = Meetings { meeting :: Meeting }
  deriving Show

instance FromJSON Meetings where
  parseJSON x = fmap Meetings (parseJSON x)
