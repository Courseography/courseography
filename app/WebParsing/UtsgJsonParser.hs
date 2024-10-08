module WebParsing.UtsgJsonParser
     (parseTimetable, insertAllMeetings) where

import Config (runDb, timetableApiUrl, reqHeaders, createReqBody)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), withObject, encode, decode, (.!=), (.:?), (.:), Object)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, Update, entityKey, deleteWhere, upsert,
                                insert, insertMany_, selectFirst, (==.), (=.))
import Database.Tables (EntityField (..), MeetTime (..), Meeting (..), buildTimes)
import Network.HTTP.Conduit (method, responseBody, requestHeaders, RequestBody(RequestBodyLBS), newManager,
                             tlsManagerSettings, httpLbs, requestBody, parseRequest)
import Data.ByteString.Lazy.Internal (ByteString)

-- | Parse all timetable data.
parseTimetable :: IO ()
parseTimetable = do
    -- Make a first request to determine the number of pages
    -- This will be used to determine the total number of requests to make
    respBody <- makeRequest 1

    let pageInfo :: Maybe (Int, Int, Int) = getPageInfo respBody
    case pageInfo of 
      Nothing -> return ()
      Just (page, pageSize, totalCourses) -> do
        liftIO $ print $ "Parsing results for page " ++ show (1 :: Int) ++ " of " ++ show totalPages
        runDb $ insertCourses respBody 
        runDb $ insertAllMeetings (page + 1) pageSize totalCourses
        where
          totalPages :: Int
          totalPages = fromIntegral (ceiling (fromIntegral totalCourses / fromIntegral pageSize :: Double) :: Int)

-- Make a request and return the response as a serialized JSON representation
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

-- Get the page number, page size and total number of courses from response
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
          
-- Helper function to insert courses for a page of a HTTP response
insertCourses :: ByteString -> SqlPersistM ()
insertCourses respBody = do
    let meetings :: Maybe DBList = decode respBody
    case meetings of
      Nothing -> return ()
      Just dblist -> do mapM_ insertMeeting $ flattenDBList dblist

-- | Helper function to flatten the list of DB Objects
flattenDBList :: DBList -> [MeetTime]
flattenDBList (DBList meetings) = concatMap (\(DB meetTimes) -> meetTimes) meetings

-- | insert/update all the data into the Meeting and Times schema by creating and sending
--   the http request to Artsci Timetable and then parsing the JSON response
insertAllMeetings :: Int -> Int -> Int -> SqlPersistM ()
insertAllMeetings page pageSize totalCourses = do
    if (page - 1) * pageSize > totalCourses  -- base case
      then liftIO $ putStrLn "All courses have been parsed."
      else do
        liftIO $ print $ "Parsing results for page " ++ show page ++ " of " ++ show totalPages
        respBody <- liftIO $ makeRequest page
        let pageInfo :: Maybe (Int, Int, Int) = getPageInfo respBody
        case pageInfo of 
          Nothing -> return ()
          Just (page', pageSize', totalCourses') -> do
            insertCourses respBody 
            insertAllMeetings (page' + 1) pageSize' totalCourses'
        where
          totalPages :: Integer
          totalPages = fromIntegral (ceiling (fromIntegral totalCourses / fromIntegral pageSize :: Double) :: Int)

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