{-|
    Module: Database.CourseQueries
    Description: Respond to various requests involving database course
                 information. Includes helpers for response functionality
                 defined in Controllers.Course

This module contains the functions that perform different database queries
and serve the information back to the client.
-}

module Database.CourseQueries
    (retrievePost,
     returnPost,
     reqsForPost,
     prereqsForCourse,
     returnMeeting,
     getMeetingTime,
     buildTime,
     getDeptCourses,
     ) where

import Config (runDb)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T (Text, append, isPrefixOf, snoc, tail, toUpper, unpack)
import Database.Persist.Sqlite (Entity, PersistValue (PersistText), SqlPersistM, entityKey,
                                entityVal, rawSql, selectFirst, selectList, (<-.), (==.))
import Database.Tables as Tables
import Happstack.Server.SimpleHTTP (Request, Response, ServerPart, askRq, ifModifiedSince,
                                    lookText')
import Models.Course (buildCourse)
import Models.Meeting (buildMeetTimes)
import Util.Happstack (createJSONResponse)

-- | Takes a http request with a post code and sends a JSON response containing the post data
-- | if the post data has been modified since the timestamp in the request,
-- | or a 304 "Not Modified" response otherwise
retrievePost :: ServerPart Response
retrievePost = do
    req <- askRq
    code <- lookText' "code"
    liftIO $ queryPost req code

-- | Queries the database for the post data then returns a JSON response of it
-- | if the post data has been modified since the timestamp in the request,
-- | or a 304 "Not Modified" response otherwise
queryPost :: Request -> T.Text -> IO Response
queryPost req code = do
    postMaybe <- returnPost code
    case postMaybe of
        Nothing -> return $ createJSONResponse (Nothing :: Maybe Post)
        Just post -> return $ ifModifiedSince (postModified post) req (createJSONResponse post)

-- | Queries the database for information about the post then returns the post value
returnPost :: T.Text -> IO (Maybe Post)
returnPost code = runDb $ do
    sqlPost <- selectFirst [PostCode ==. code] []
    case sqlPost of
        Nothing -> return Nothing
        Just post -> return $ Just $ entityVal post

-- | Retrieves the course requirements for a Post as a list of course codes
reqsForPost :: Post -> [String]
reqsForPost post = do
    let requirementsText = T.unpack $ postRequirements post
        cleaned = filter (`notElem` ("<>" :: String)) $ filter (not . isPunctuation) requirementsText
        potentialCodes = words cleaned
    filter isCourseCode potentialCodes
  where
    -- | TODO: change function to use a regex
    isCourseCode :: String -> Bool
    isCourseCode codeStr =
        length codeStr == 8 &&
        all isAlphaNum codeStr &&
        all isAlpha (take 3 codeStr) &&
        all isDigit (take 3 (drop 3 codeStr)) &&
        isAlpha (codeStr !! 6) &&
        isDigit (codeStr !! 7)

-- | Queries the database for all information regarding a specific meeting for
--  a @course@, returns a Meeting.
returnMeeting :: T.Text -> T.Text -> T.Text -> SqlPersistM (Maybe (Entity Meeting))
returnMeeting lowerStr sect session = do
    selectFirst [MeetingCode ==. T.toUpper lowerStr,
                                  MeetingSection ==. sect,
                                  MeetingSession ==. session]
                                 []

-- ** Other queries

-- | Retrieves the prerequisites for a course (code) as a string.
-- Also retrieves the actual course code in the database in case
-- the one the user inputs doesn't match it exactly
prereqsForCourse :: T.Text -> IO (Either String (T.Text, T.Text))
prereqsForCourse courseCode = runDb $ do
    let upperCaseCourseCode = T.toUpper courseCode
    course <- selectFirst [CoursesCode <-. [upperCaseCourseCode, upperCaseCourseCode `T.append` "H1", upperCaseCourseCode `T.append` "Y1"]] []
    case course of
        Nothing -> return (Left "Course not found")
        Just courseEntity ->
            return (Right
                     (coursesCode $ entityVal courseEntity,
                      fromMaybe "" $ coursesPrereqString $ entityVal courseEntity)
                    ) :: SqlPersistM (Either String (T.Text, T.Text))

getDeptCourses :: MonadIO m => T.Text -> m [Course]
getDeptCourses dept = liftIO $ runDb $ do
        courses :: [Entity Courses] <- rawSql "SELECT ?? FROM courses WHERE code LIKE ?" [PersistText $ T.snoc dept '%']
        let deptCourses = map entityVal courses
        meetings :: [Entity Meeting] <- selectList [MeetingCode <-. map coursesCode deptCourses] []
        mapM (processCourse meetings) deptCourses
    where
        processCourse allMeetings course = do
            let courseMeetings = filter (\m -> meetingCode (entityVal m) == coursesCode course) allMeetings
            allTimes <- mapM buildMeetTimes courseMeetings
            buildCourse allTimes course

-- | Queries the database for all times regarding a specific meeting (lecture, tutorial or practial) for
-- a @course@, returns a list of Time.
getMeetingTime :: (T.Text, T.Text, T.Text) -> SqlPersistM [Time]
getMeetingTime (meetingCode_, meetingSection_, meetingSession_) = do
    maybeEntityMeetings <- selectFirst [MeetingCode ==. meetingCode_,
                                        MeetingSection ==. getMeetingSection meetingSection_,
                                        MeetingSession ==. meetingSession_]
                                       []
    allTimes <- selectList [TimesMeeting ==. entityKey (fromJust maybeEntityMeetings)] []
    mapM (buildTime . entityVal) allTimes

getMeetingSection :: T.Text -> T.Text
getMeetingSection sec
    | T.isPrefixOf "L" sec = T.append "LEC" sectCode
    | T.isPrefixOf "T" sec = T.append "TUT" sectCode
    | T.isPrefixOf "P" sec = T.append "PRA" sectCode
    | otherwise            = sec
    where sectCode = T.tail sec
