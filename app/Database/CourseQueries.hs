{-|
    Module: Database.CourseQueries
    Description: Respond to various requests involving database course
                 information. Includes helpers for response functionality
                 defined in Controllers.Course

This module contains the functions that perform different database queries
and serve the information back to the client.
-}

module Database.CourseQueries
    (returnPost,
     reqsForPost,
     prereqsForCourse,
     returnMeeting,
     getGraph,
     getMeetingTime,
     buildTime,
     getDeptCourses,
     buildCourse,
     buildMeetTimes
     ) where

import Config (runDb)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value, object, toJSON)
import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation)
import Data.List (partition)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T (Text, append, filter, isPrefixOf, snoc, tail, toUpper, unpack)
import Database.DataType (ShapeType (BoolNode, Hybrid, Node))
import Database.Persist.Sqlite (Entity, PersistEntity, PersistValue (PersistInt64, PersistText),
                                SqlPersistM, entityKey, entityVal, get, keyToValues, rawSql,
                                selectFirst, selectList, (<-.), (==.))
import Database.Tables as Tables
import Svg.Builder (buildEllipses, buildPath, buildRect, intersectsWithShape)

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
returnMeeting :: T.Text -> T.Text -> T.Text -> SqlPersistM (Entity Meeting)
returnMeeting lowerStr sect session = do
    entityMeetings <- selectList [MeetingCode ==. T.toUpper lowerStr,
                                  MeetingSection ==. sect,
                                  MeetingSession ==. session]
                                 []
    return $ head entityMeetings

-- | Builds a Course structure from a tuple from the Courses table.
-- Some fields still need to be added in.
buildCourse :: [MeetTime'] -> Courses -> SqlPersistM Course
buildCourse allMeetings course = do
    cBreadth <- getDescriptionB (coursesBreadth course)
    cDistribution <- getDescriptionD (coursesDistribution course)
    return $ Course cBreadth
           -- TODO: Remove the filter and allow double-quotes
           (fmap (T.filter (/='\"')) (coursesDescription course))
           (fmap (T.filter (/='\"')) (coursesTitle course))
           (coursesPrereqString course)
           (Just allMeetings)
           (coursesCode course)
           (coursesExclusions course)
           cDistribution
           (coursesCoreqs course)
           (coursesVideoUrls course)

-- | Queries the database for the breadth description
getDescriptionB :: Maybe (Key Breadth) -> SqlPersistM (Maybe T.Text)
getDescriptionB Nothing = return Nothing
getDescriptionB (Just key) = do
    maybeBreadth <- get key
    return $ fmap breadthDescription maybeBreadth

-- | Queries the database for the distribution description
getDescriptionD :: Maybe (Key Distribution) -> SqlPersistM (Maybe T.Text)
getDescriptionD Nothing = return Nothing
getDescriptionD (Just key) = do
    maybeDistribution <- get key
    return $ fmap distributionDescription maybeDistribution

-- | Queries the database for all times corresponding to a given meeting.
buildMeetTimes :: Entity Meeting -> SqlPersistM Tables.MeetTime'
buildMeetTimes meet = do
    allTimes :: [Entity Times] <- selectList [TimesMeeting ==. entityKey meet] []
    parsedTime <- mapM (buildTime . entityVal) allTimes
    return $ Tables.MeetTime' (entityVal meet) parsedTime

-- ** Other queries

getGraph :: T.Text -> IO (Maybe Value)
getGraph graphName = runDb $ do
    graphEnt :: (Maybe (Entity Graph)) <- selectFirst [GraphTitle ==. graphName] []
    case graphEnt of
        Nothing -> return Nothing
        Just graph -> do
            let gId = entityKey graph
            sqlTexts    :: [Entity Text] <- selectList [TextGraph ==. gId] []
            sqlRects    :: [Entity Shape] <- selectList
                                                 [ShapeType_ <-. [Node, Hybrid],
                                                  ShapeGraph ==. gId] []
            sqlEllipses :: [Entity Shape] <- selectList
                                                 [ShapeType_ ==. BoolNode,
                                                  ShapeGraph ==. gId] []
            sqlPaths    :: [Entity Path] <- selectList [PathGraph ==. gId] []

            let
                keyAsInt :: PersistEntity a => Entity a -> Integer
                keyAsInt = fromIntegral . (\(PersistInt64 x) -> x) . head . keyToValues . entityKey

                graphtexts          = map entityVal sqlTexts
                rects          = zipWith (buildRect graphtexts)
                                         (map entityVal sqlRects)
                                         (map keyAsInt sqlRects)
                ellipses       = zipWith (buildEllipses graphtexts)
                                         (map entityVal sqlEllipses)
                                         (map keyAsInt sqlEllipses)
                graphpaths     = zipWith (buildPath rects ellipses)
                                         (map entityVal sqlPaths)
                                         (map keyAsInt sqlPaths)
                (regions, _)   = partition pathIsRegion graphpaths
                regionTexts    = filter (not .
                                         intersectsWithShape (rects ++ ellipses))
                                        graphtexts

                response = object [
                        ("texts", toJSON $ graphtexts ++ regionTexts),
                        ("shapes", toJSON $ rects ++ ellipses),
                        ("paths", toJSON $ graphpaths ++ regions),
                        ("width", toJSON $ graphWidth $ entityVal graph),
                        ("height", toJSON $ graphHeight $ entityVal graph)
                    ]

            return (Just response)

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
