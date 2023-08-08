module WebParsing.UtsgJsonParser
     (parseTimetable, insertAllMeetings) where

import Config (databasePath, timetableApiUrl)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), withObject, Value (..), encode, decode, object, (.=), (.!=), (.:?), (.:))
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, Update, Entity, entityKey, entityVal, deleteWhere, upsert,
                                insert, insertMany_, runSqlite, selectFirst, selectList, (==.), (=.))
import Database.Tables (Courses (..), EntityField (..), MeetTime (..), Meeting (..), buildTimes)
import Network.HTTP.Conduit

-- | Parse all timetable data.
parseTimetable :: IO ()
parseTimetable = do
    orgs <- getOrgs
    runSqlite databasePath $ mapM_ insertAllMeetings orgs

-- | Get all the orgs from the Courses schema in the database
getOrgs :: IO [T.Text]
getOrgs = runSqlite databasePath $ do
    courseEntities <- selectList [] [] :: SqlPersistM [Entity Courses]
    let courseCodes = map (coursesCode . entityVal) courseEntities
    let orgsSet = Set.fromList $ map (T.take 3) courseCodes
    return $ Set.toList orgsSet

-- | insert/update all the data into the Meeting and Times schema by creating and sending
--   the http request to Artsci Timetable and then parsing the JSON response
insertAllMeetings :: T.Text -> SqlPersistM ()
insertAllMeetings org = do
    liftIO . print $ T.append "parsing JSON data from: " org

    -- Define the request
    let reqBody = object [ "campuses" .= ([] :: [T.Text])
                                , "courseCodeAndTitleProps" .= object
                                    [ "courseCode" .= ("" :: T.Text)
                                    , "courseSectionCode" .= ("" :: T.Text)
                                    , "courseTitle" .= org
                                    , "searchCourseDescription" .= True
                                    ]
                                , "courseLevels" .= ([] :: [T.Text])
                                , "creditWeights" .= ([] :: [T.Text])
                                , "dayPreferences" .= ([] :: [T.Text])
                                , "deliveryModes" .= ([] :: [T.Text])
                                , "departmentProps" .= ([] :: [T.Text])
                                , "direction" .= ("asc" :: T.Text)
                                , "divisions" .= [T.pack "ARTSC"]
                                , "instructor" .= ("" :: T.Text)
                                , "page" .= (1 :: Int)
                                , "pageSize" .= (200 :: Int)
                                , "requirementProps" .= ([] :: [T.Text])
                                , "sessions" .= [T.pack "20239", T.pack "20241", T.pack "20239-20241"]
                                , "timePreferences" .= ([] :: [T.Text])
                                ]
        reqHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")]
    request <- liftIO $ parseRequest (T.unpack timetableApiUrl)
    let request' = request { method = "POST"
                           , requestBody = RequestBodyLBS $ encode reqBody
                           , requestHeaders = reqHeaders
                           }

    -- Make the request
    manager <- liftIO $ newManager tlsManagerSettings
    response <- liftIO $ httpLbs request' manager
    let respBody = responseBody response

    -- decode the response
    let meetings :: Maybe DBList = decode respBody
    case meetings of
        Nothing -> return ()
        Just (DBList dbs) -> forM_ dbs $ \(DB meetTimes) -> mapM_ insertMeeting meetTimes

-- | Insert or update a meeting and then delete
--   and re-insert the corresponding Times into the database.
insertMeeting :: MeetTime -> SqlPersistM ()
insertMeeting (MeetTime meetingData meetingTime) = do
    -- Check that the meeting belongs to a course that exists
    let code = meetingCode meetingData
    courseKey <- selectFirst [ MeetingCode ==. code ] []
    case courseKey of
        Just _ -> do -- course already exists, so update/replace
          entity <- upsert meetingData (meetingUpdates meetingData)
          let meetingKey = entityKey entity
          deleteWhere [ TimesMeeting ==. meetingKey ]
          let allTimes = map (buildTimes meetingKey) meetingTime
          insertMany_ allTimes
        Nothing -> do -- course does not exist, so insert
          meetingKey <- insert meetingData
          let allTimes = map (buildTimes meetingKey) meetingTime
          insertMany_ allTimes

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

newtype DB = DB { dbData :: [MeetTime]}
  deriving Show

newtype DBList = DBList [DB]
  deriving Show

instance FromJSON DBList where
  parseJSON = withObject "Expected an Object for DBList" $ \o -> do
    maybeOrg <- o .:? "payload"
    case maybeOrg of
      Just payload -> do
        pageableCourse <- payload .: "pageableCourse"
        courses <- pageableCourse .: "courses"
        dbList <- mapM parseCourse courses
        return $ DBList dbList
        where
          parseCourse courseObj = do
            course <- parseJSON (Object courseObj)
            session :: T.Text <- courseObj .:? "sectionCode" .!= "F"
            sectionsList :: [MeetTime] <- courseObj .:? "sections" .!= []
            let finalSectionsList = map (\m -> m { meetInfo = (meetInfo m) { meetingCode = coursesCode course, meetingSession = session } }) sectionsList
            return $ DB finalSectionsList
      Nothing -> return $ DBList []
