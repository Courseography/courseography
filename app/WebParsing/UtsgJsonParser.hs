module WebParsing.UtsgJsonParser (parseTimetable, insertTimetableData) where

import Config (createReqBody, reqHeaders, runDb, timetableApiUrl)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, decode, encode, (.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Default.Class (def)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM)
import Database.Tables (Meeting (..))
import Models.Meeting (MeetingParsedData (..), insertMeetingParsedData)

import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Conduit (
    RequestBody (RequestBodyLBS),
    httpLbs,
    method,
    mkManagerSettings,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
 )
import Network.TLS (EMSMode (AllowEMS), Supported (..))

-- | Parse all timetable data.
parseTimetable :: IO ()
parseTimetable = do
    runDb $ insertTimetablePages 1

-- Make a request and return the response as a serialized JSON representation
makeRequest :: Int -> IO ByteString
makeRequest pageNum = do
    -- set up the request
    let reqBody = createReqBody pageNum
    timetableApi <- liftIO timetableApiUrl
    request <- liftIO $ parseRequest (T.unpack timetableApi)
    let request' = request{method = "POST", requestBody = RequestBodyLBS $ encode reqBody, requestHeaders = reqHeaders}

    -- make the request
    manager <-
        liftIO $
            newManager $
                mkManagerSettings (TLSSettingsSimple False False False (def{supportedExtendedMainSecret = AllowEMS})) Nothing
    response <- liftIO $ httpLbs request' manager
    return $ responseBody response

-- | Helper function to insert courses for a page of a HTTP response
insertTimetableData :: ByteString -> SqlPersistM ()
insertTimetableData respBody =
    case decode respBody >>= parseMaybe parseCourses of
        Nothing -> liftIO $ print ("Failed to parse meeting information." :: String)
        Just meetings -> mapM_ insertMeetingParsedData meetings
  where
    -- Parse all of the meetings and times for the courses in a page
    parseCourses :: Object -> Parser [MeetingParsedData]
    parseCourses obj = do
        payload <- obj .: "payload"
        pageableCourse <- payload .: "pageableCourse"
        rawCoursesData :: [Object] <- pageableCourse .: "courses"
        concat <$> mapM parseCourse rawCoursesData

    -- Parse the meetings and times for a single course
    parseCourse :: Object -> Parser [MeetingParsedData]
    parseCourse o = do
        codeExtraChars <- o .: "code"
        let courseCode = T.dropEnd 2 codeExtraChars
        session :: T.Text <- o .: "sectionCode"
        meetingsParsedData :: [MeetingParsedData] <- o .:? "sections" .!= []
        return $ map (\m -> m{meetInfo = (meetInfo m){meetingCode = courseCode, meetingSession = session}}) meetingsParsedData

-- | Retrieve timetable information for @page@ and insert/update the corresponding Meeting
--   and Times data into the database. Repeat for all pages in increasing order.
insertTimetablePages :: Int -> SqlPersistM ()
insertTimetablePages page = do
    respBody <- liftIO $ makeRequest page
    let pageInfo = getPageInfo respBody
    case pageInfo of
        Nothing -> return ()
        Just (_, pageSize, totalCourses) -> do
            let totalPages :: Integer = ceiling (fromIntegral totalCourses / fromIntegral pageSize :: Double)
            liftIO $ print $ "Parsing results for page " ++ show page ++ " of " ++ show totalPages
            insertTimetableData respBody

            if page * pageSize >= totalCourses
                then liftIO $ print ("All courses have been parsed." :: String)
                else insertTimetablePages (page + 1)
  where
    -- Get the page number, page size and total number of courses from response
    getPageInfo :: ByteString -> Maybe (Int, Int, Int)
    getPageInfo respBody = do
        json <- decode respBody
        flip parseMaybe json $ \obj -> do
            payload <- obj .: "payload"
            pageableCourse <- payload .: "pageableCourse"
            pageNum <- pageableCourse .: "page"
            pageSize <- pageableCourse .: "pageSize"
            totalCourses <- pageableCourse .: "total"
            return (pageNum, pageSize, totalCourses)
