module WebParsing.UtsgJsonParser
     (parseTimetable, insertAllMeetings) where

import Config (runDb, timetableApiUrl, reqHeaders, createReqBody)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), withObject, encode, decode, (.!=), (.:?), (.:))
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, Update, Entity, entityKey, entityVal, deleteWhere, upsert,
                                insert, insertMany_, selectFirst, selectList, (==.), (=.))
import Database.Tables (Courses (..), EntityField (..), MeetTime (..), Meeting (..), buildTimes)
import Network.HTTP.Conduit (method, responseBody, requestHeaders, RequestBody(RequestBodyLBS), newManager,
                             tlsManagerSettings, httpLbs, requestBody, parseRequest)

-- | Parse all timetable data.
parseTimetable :: IO ()
parseTimetable = do
    orgs <- getOrgs
    runDb $ mapM_ insertAllMeetings orgs

-- | Get all the orgs from the courses table in the database
getOrgs :: IO [T.Text]
getOrgs = do
    runDb $ do
        courseEntities <- selectList [] [] :: SqlPersistM [Entity Courses]
        let courseCodes = map (coursesCode . entityVal) courseEntities
        let orgsSet = Set.fromList $ map (T.take 3) courseCodes
        return $ Set.toList orgsSet

-- | insert/update all the data into the Meeting and Times schema by creating and sending
--   the http request to Artsci Timetable and then parsing the JSON response
insertAllMeetings :: T.Text -> SqlPersistM ()
insertAllMeetings org = do
    liftIO . print $ T.append "parsing JSON data from: " org

    -- set up the request
    let reqBody = createReqBody org
    timetableApi <- liftIO timetableApiUrl
    request <- liftIO $ parseRequest (T.unpack timetableApi)
    let request' = request {method = "POST", requestBody = RequestBodyLBS $ encode reqBody, requestHeaders = reqHeaders}

    -- make the request
    manager <- liftIO $ newManager tlsManagerSettings
    response <- liftIO $ httpLbs request' manager
    let respBody = responseBody response

    -- decode the response
    let meetings :: Maybe DBList = decode respBody
    case meetings of
      Nothing -> return ()
      Just dblist ->  mapM_ insertMeeting $ flattenDBList dblist

-- | Helper function to flatten the list of DB Objects
flattenDBList :: DBList -> [MeetTime]
flattenDBList (DBList meetings) = concatMap (\(DB meetTimes) -> meetTimes) meetings

-- | Insert or update a meeting and then delete
--   and re-insert the corresponding Times into the database.
insertMeeting :: MeetTime -> SqlPersistM ()
insertMeeting (MeetTime meetingData meetingTime) = do
    -- Check if the meeting already exists in the meeting table
    let code = meetingCode meetingData
    let session = meetingSession meetingData
    let section = meetingSection meetingData
    maybeMeetingKey <- selectFirst [MeetingCode ==. code, MeetingSession ==. session, MeetingSection ==. section] []
    case maybeMeetingKey of
        Just _ -> do -- meeting already exists, so update/replace
          entity <- upsert meetingData (meetingUpdates meetingData)
          let meetingKey = entityKey entity
          deleteWhere [ TimesMeeting ==. meetingKey ]
          let allTimes = map (buildTimes meetingKey) meetingTime
          insertMany_ allTimes
        Nothing -> do -- meeting does not exist, so insert
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

instance FromJSON DB where
  parseJSON = withObject "Expected an Object for DB" $ \o -> do
    codeExtraChars <- o .: "code"
    let courseCode = T.dropEnd 2 codeExtraChars
    session :: T.Text <- o .: "sectionCode"
    sectionsList :: [MeetTime] <- o .:? "sections" .!= []
    let finalSectionsList = map (\m -> m { meetInfo = (meetInfo m) { meetingCode = courseCode, meetingSession = session } }) sectionsList
    return $ DB finalSectionsList

newtype DBList = DBList [DB]
  deriving Show

instance FromJSON DBList where
  parseJSON = withObject "Expected an Object for DBList" $ \o -> do
    maybeOrg <- o .:? "payload"
    case maybeOrg of
      Just payload -> do
        pageableCourse <- payload .: "pageableCourse"
        courses <- pageableCourse .: "courses"
        dbList <- mapM parseJSON courses
        return $ DBList dbList
      Nothing -> return $ DBList []
