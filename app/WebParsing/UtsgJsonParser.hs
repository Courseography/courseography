module WebParsing.UtsgJsonParser
     (parseTimetable, insertAllMeetings) where

import Config (runDb, timetableApiUrl, reqHeaders, createReqBody)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), withObject, encode, decode, (.!=), (.:?), (.:), Value (..), Object, Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, Update, Entity, entityKey, entityVal, deleteWhere, upsert,
                                insert, insertMany_, selectFirst, selectList, (==.), (=.))
import Database.Tables (Courses (..), EntityField (..), MeetTime (..), Meeting (..), buildTimes)
import Network.HTTP.Conduit (method, responseBody, requestHeaders, RequestBody(RequestBodyLBS), newManager,
                             tlsManagerSettings, httpLbs, requestBody, parseRequest)
import Network.HTTP.Client (Response(responseBody))
import DynamicGraphs.GraphOptions (CourseGraphOptions(courses))

import Data.ByteString.Lazy.Internal (ByteString)
import Text.LaTeX (pagebreak)
-- import Happstack.Server (decodeBody)

-- | Parse all timetable data.
parseTimetable :: IO ()
parseTimetable = do
    -- runDb $ mapM_ insertAllMeetings [1]
    
    -- (page, pageSize, totalCourses) <- parseTimetable 1
    -- insertAllMeetings page pageSize totalCourses
    respBody <- makeRequest 1
    -- liftIO $ print respBody

    let pageInfo :: Maybe (Int, Int, Int) = getPageInfo respBody
    case pageInfo of 
      Nothing -> return ()
      Just (page, pageSize, totalCourses) -> do
        -- insert the meetings for page 1
        -- let meetings :: Maybe DBList = decode respBody
        -- case meetings of
        --   Nothing -> return ()
        --   Just dblist -> do mapM_ insertMeeting $ flattenDBList dblist
        runDb $ helper respBody 

        runDb $ insertAllMeetings (page + 1) pageSize totalCourses

helper :: ByteString -> SqlPersistM ()
helper respBody = do
    let meetings :: Maybe DBList = decode respBody
    case meetings of
      Nothing -> return ()
      Just dblist -> do mapM_ insertMeeting $ flattenDBList dblist

   

-- parseTimatablePage :: Int -> IO (Int, Int, Int)
-- parseTimatablePage pageNum = do
--     -- set up the request
--     let reqBody = createReqBody pageNum
--     liftIO $ print reqBody

--     timetableApi <- liftIO timetableApiUrl
--     request <- liftIO $ parseRequest (T.unpack timetableApi)
--     let request' = request {method = "POST", requestBody = RequestBodyLBS $ encode reqBody, requestHeaders = reqHeaders}

--     -- make the request
--     manager <- liftIO $ newManager tlsManagerSettings
--     response <- liftIO $ httpLbs request' manager
--     let respBody = responseBody response

--     -- decode the response
--     let meetings :: Maybe DBList = decode respBody
--     case meetings of
--       Nothing -> return ()
--       Just dblist -> do mapM_ insertMeeting $ flattenDBList dblist

--     -- let pageInfo :: Maybe (Int, Int, Int) = getPageInfo respBody
--     let pageInfo :: Maybe (Int, Int, Int) = getPageInfo respBody
--     case pageInfo of 
--       Nothing -> return (-1, -1, -1)
--       Just info -> return info

makeRequest :: Int -> IO ByteString
makeRequest pageNum = do 
    -- set up the request
    let reqBody = createReqBody pageNum
    timetableApi <- liftIO timetableApiUrl
    request <- liftIO $ parseRequest (T.unpack timetableApi)
    let request' = request {method = "POST", requestBody = RequestBodyLBS $ encode reqBody, requestHeaders = reqHeaders}

    -- make the request
    manager <- liftIO $ newManager tlsManagerSettings
    response <- liftIO $ httpLbs request' manager
    let respBody = responseBody response
    return respBody

    -- -- decode the response 
    -- let decodedJson :: Maybe Object = decode respBody
    -- case decodedJson of 
    --   Nothing -> return Nothing
    --   Just json -> return $ Just json
    
getPageInfo :: ByteString -> Maybe (Int, Int, Int)
getPageInfo respBody = do
    jsonValue <- decode respBody
    case jsonValue of 
      Nothing -> return (-1, -1, -1)
      Just json -> do 
        flip parseMaybe json $ \obj -> do
          payload :: Object <- obj .: "payload"
          pageableCourse :: Object <- payload .: "pageableCourse"
          page :: Int <- pageableCourse .: "page"
          pageSize :: Int <- pageableCourse .: "pageSize"
          totalCourses :: Int <- pageableCourse .: "total"
          return (page, pageSize, totalCourses)

-- | insert/update all the data into the Meeting and Times schema by creating and sending
--   the http request to Artsci Timetable and then parsing the JSON response
insertAllMeetings :: Int -> Int -> Int -> SqlPersistM ()
insertAllMeetings page pageSize totalCourses = do
    if (page - 1) * pageSize > totalCourses  -- base case
      then liftIO $ print "All courses have been parsed."
      else do
        liftIO $ print $ "Parsing results for page " ++ show page
        respBody <- liftIO $ makeRequest page
        let pageInfo :: Maybe (Int, Int, Int) = getPageInfo respBody
        case pageInfo of 
          Nothing -> return ()
          Just (page, pageSize, totalCourses) -> do
            helper respBody 
            insertAllMeetings (page + 1) pageSize totalCourses

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