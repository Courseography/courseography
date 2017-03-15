{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}

{-|
    Module: Database.CourseQueries
    Description: Respond to various requests involving database course
                 information.

This module contains the functions that perform different database queries
and serve the information back to the client.
-}

module Database.CourseQueries
    (retrieveCourse,
     returnCourse,
     allCourses,
     courseInfo,
     getDeptCourses,
     queryGraphs,
     deptList,
     returnMeeting,
     getGraphJSON,
     getMeetingTime) where

import Happstack.Server.SimpleHTTP
import Database.Persist
import Database.Persist.Sqlite
import Database.Tables as Tables
import Control.Monad.IO.Class (liftIO, MonadIO)
import Util.Happstack (createJSONResponse)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import WebParsing.ParsingHelp
import Data.String.Utils
import Data.List
import Config (databasePath)
import Control.Monad (liftM)
import Data.Aeson ((.=), toJSON, object)
import Database.DataType
import Svg.Builder


-- | Queries the database for all matching lectures, tutorials,
--   or praticals of this course.
meetingQuery :: T.Text -> SqlPersistM [Entity Meeting]
meetingQuery meetingCode = selectList [MeetingCode ==. meetingCode] []

splitSessions :: [Entity Meeting] -> ([Entity Meeting], [Entity Meeting], [Entity Meeting])
splitSessions meetingsList =
    let fallM = filter (\m -> meetingSession (entityVal m) == "F") meetingsList
        springM = filter (\m -> meetingSession (entityVal m) == "S") meetingsList
        yearM = filter (\m -> meetingSession (entityVal m) == "Y") meetingsList
    in (fallM, springM, yearM)

-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO Course
returnCourse lowerStr = runSqlite databasePath $ do
    let courseStr = T.toUpper lowerStr
    sqlCourse :: (Maybe (Entity Courses)) <- selectFirst [CoursesCode ==. courseStr] []
    case sqlCourse of
      Nothing -> return emptyCourse
      Just course -> do
        meetings <- meetingQuery courseStr
        let (fall, spring, year) = buildAllSessions meetings
        buildCourse (Just fall)
                    (Just spring)
                    (Just year)
                    (entityVal course)

buildAllSessions :: [Entity Meeting] -> (Tables.Session, Tables.Session, Tables.Session)
buildAllSessions entityListM =
    let (fallM, springM, yearM) = splitSessions entityListM
        fall = buildSession fallM
        spring = buildSession springM
        year = buildSession yearM
    in (fall, spring, year)

-- | Takes a course code (e.g. \"CSC108H1\") and sends a JSON representation
-- of the course as a response.
retrieveCourse :: T.Text -> ServerPart Response
retrieveCourse = liftIO . queryCourse

-- | Queries the database for all information about @course@, constructs a JSON object
-- representing the course and returns the appropriate JSON response.
queryCourse :: T.Text -> IO Response
queryCourse str = do
    courseJSON <- returnCourse str
    return $ createJSONResponse courseJSON

-- | Queries the database for all information regarding a specific meeting for
--  a @course@, returns a Meeting.
returnMeeting :: T.Text -> T.Text -> T.Text -> SqlPersistM (Maybe Meeting)
returnMeeting lowerStr sect session = do
    maybeEntityMeetings <- selectFirst [MeetingCode ==. T.toUpper lowerStr,
                                        MeetingSection ==. sect,
                                        MeetingSession ==. session]
                                       []
    return $ fmap entityVal maybeEntityMeetings

-- | Builds a Course structure from a tuple from the Courses table.
-- Some fields still need to be added in.
buildCourse :: Maybe Session -> Maybe Session -> Maybe Session -> Courses -> SqlPersistM Course
buildCourse fall spring year course = do
    cBreadth <- getDescriptionB (coursesBreadth course)
    cDistribution <- getDescriptionD (coursesDistribution course)
    return $ Course cBreadth
           -- TODO: Remove the filter and allow double-quotes
           (fmap (T.filter (/='\"')) (coursesDescription course))
           (fmap (T.filter (/='\"')) (coursesTitle course))
           (coursesPrereqString course)
           fall
           spring
           year
           (coursesCode course)
           (coursesExclusions course)
           (coursesManualTutorialEnrolment course)
           (coursesManualPracticalEnrolment course)
           cDistribution
           (coursesPrereqs course)
           (coursesCoreqs course)
           (coursesVideoUrls course)


getDescriptionB :: Maybe (Key Breadth) -> SqlPersistM (Maybe T.Text)
getDescriptionB Nothing = return Nothing
getDescriptionB (Just key) = do
    maybeBreadth <- get key
    case maybeBreadth of
        Nothing -> return Nothing
        Just coursebreadth  -> return $ Just $ breadthDescription coursebreadth

getDescriptionD :: Maybe (Key Distribution) -> SqlPersistM (Maybe T.Text)
getDescriptionD Nothing = return Nothing
getDescriptionD (Just key) = do
    maybeDistribution <- get key
    case maybeDistribution of
        Nothing -> return Nothing
        Just dist -> return $ Just $ distributionDescription dist

-- | Builds a Session structure from three lists of tuples from the Meeting table,
-- representing information for lectures, tutorials and practicals.
buildSession :: [Entity Meeting] -> Tables.Session
buildSession = buildSession' . map entityVal

buildSession' :: [Meeting] -> Tables.Session
buildSession' meetings =
    let lecs = filter (\m -> (T.head $ meetingSection m) == 'L') meetings
        tuts = filter (\m -> (T.head $ meetingSection m) == 'T') meetings
        pras = filter (\m -> (T.head $ meetingSection m) == 'P') meetings
    in Tables.Session lecs tuts pras

-- ** Other queries

-- | Looks up a graph using its title then gets the Shape, Text and Path elements
-- for rendering graph (returned as JSON).
getGraphJSON :: T.Text -> IO Response
getGraphJSON graphName =
    runSqlite databasePath $ do
        graphEnt :: (Maybe (Entity Graph)) <- selectFirst [GraphTitle ==. graphName] []
        case graphEnt of
            Nothing -> return $ createJSONResponse $ object ["texts" .= ([] :: [Text]),
                                                             "shapes" .= ([] :: [Shape]),
                                                             "paths" .= ([] :: [Path])]
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

                    response = createJSONResponse $
                        object [
                            ("texts", toJSON $ graphtexts ++ regionTexts),
                            ("shapes", toJSON $ rects ++ ellipses),
                            ("paths", toJSON $ graphpaths ++ regions),
                            ("width", toJSON $ graphWidth $ entityVal graph),
                            ("height", toJSON $ graphHeight $ entityVal graph)
                        ]

                return response :: SqlPersistM Response

-- | Builds a list of all course codes in the database.
allCourses :: IO Response
allCourses = do
  response <- runSqlite databasePath $ do
      courses :: [Entity Courses] <- selectList [] []
      let codes = map (coursesCode . entityVal) courses
      return $ T.unlines codes :: SqlPersistM T.Text
  return $ toResponse response

-- | Returns all course info for a given department.
courseInfo :: T.Text -> ServerPart Response
courseInfo dept = liftM createJSONResponse (getDeptCourses dept)

-- | Returns all course info for a given department.
getDeptCourses :: MonadIO m => T.Text -> m [Course]
getDeptCourses dept =
    liftIO $ runSqlite databasePath $ do
        courses  :: [Entity Courses]  <- selectList [] []
        meetings :: [Entity Meeting]  <- selectList [] []
        let c = filter (T.isPrefixOf dept . coursesCode) $ map entityVal courses
        mapM (buildTimes (map entityVal meetings)) c
    where
        meetingByCode course = filter (\m -> meetingCode m == coursesCode course)
        buildTimes meetings course =
            let fallMeetings = filter (\lec -> meetingSession lec == "F") meetings
                springMeetings = filter (\lec -> meetingSession lec == "S") meetings
                yearMeetings = filter (\lec -> meetingSession lec == "Y") meetings
                fall   = buildSession' (meetingByCode course fallMeetings)
                spring = buildSession' (meetingByCode course springMeetings)
                year   = buildSession' (meetingByCode course yearMeetings)
            in
                buildCourse (Just fall)
                            (Just spring)
                            (Just year)
                            course

-- | Return a list of all departments.
deptList :: IO Response
deptList = do
    depts <- runSqlite databasePath $ do
        courses :: [Entity Courses] <- selectList [] []
        return $ sort . nub $ map g courses :: SqlPersistM [[Char]]
    return $ createJSONResponse depts
    where
        g = take 3 . T.unpack . coursesCode . entityVal

-- | Queries the graphs table and returns a JSON response of Graph JSON
-- objects.
queryGraphs :: IO Response
queryGraphs = runSqlite databasePath $ do
    graphs :: [Entity Graph] <- selectList [] [Asc GraphTitle]
    return $ createJSONResponse graphs :: SqlPersistM Response

-- | Queries the database for all times regarding a specific meeting (lecture, tutorial or practial) for
-- a @course@, returns a list of Time.
getMeetingTime :: (T.Text, T.Text, T.Text) -> SqlPersistM [Time]
getMeetingTime (meetingCode, meetingSection, meetingSession) = do
    maybeEntityMeetings <- selectFirst [MeetingCode ==. meetingCode,
                                        MeetingSection ==. getMeetingSection meetingSection,
                                        MeetingSession ==. meetingSession]
                                       []
    return $ maybe [] (meetingTimes . entityVal) maybeEntityMeetings

getMeetingSection :: T.Text -> T.Text
getMeetingSection sec
    | T.isPrefixOf "L" sec = T.append "LEC" sectCode
    | T.isPrefixOf "T" sec = T.append "TUT" sectCode
    | T.isPrefixOf "P" sec = T.append "PRA" sectCode
    | otherwise            = sec
    where sectCode = T.tail sec
