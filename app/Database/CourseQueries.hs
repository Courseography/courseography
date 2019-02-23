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
     getMeetingTime,
     buildTimes') where

import Happstack.Server.SimpleHTTP
import Database.Persist
import Database.Persist.Sqlite
import Database.Tables as Tables
import Control.Monad.IO.Class (liftIO, MonadIO)
import Util.Happstack (createJSONResponse)
import qualified Data.Text as T
import Data.List
import Config (databasePath)
import Data.Aeson ((.=), toJSON, object)
import Database.DataType
import Svg.Builder
import Data.Maybe

-- | Queries the database for all matching lectures, tutorials,
meetingQuery :: T.Text -> SqlPersistM [MeetTime]
meetingQuery meetingCode_ = do
    allMeetings <- selectList [MeetingCode ==. meetingCode_] []
    mapM buildMeetTimes allMeetings

-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO (Maybe Course)
returnCourse lowerStr = runSqlite databasePath $ do
    let courseStr = T.toUpper lowerStr
    sqlCourse :: (Maybe (Entity Courses)) <- selectFirst [CoursesCode ==. courseStr] []
    case sqlCourse of
      Nothing -> return Nothing
      Just course -> do
        meetings <- meetingQuery courseStr
        fmap Just $ buildCourse meetings
                                (entityVal course)

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
returnMeeting :: T.Text -> T.Text -> T.Text -> SqlPersistM (Entity Meeting)
returnMeeting lowerStr sect session = do
    entityMeetings <- selectList [MeetingCode ==. T.toUpper lowerStr,
                                  MeetingSection ==. sect,
                                  MeetingSession ==. session]
                                 []
    return $ head entityMeetings

-- | Builds a Course structure from a tuple from the Courses table.
-- Some fields still need to be added in.
buildCourse :: [MeetTime] -> Courses -> SqlPersistM Course
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
           (coursesManualTutorialEnrolment course)
           (coursesManualPracticalEnrolment course)
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
buildMeetTimes :: Entity Meeting -> SqlPersistM Tables.MeetTime
buildMeetTimes meet = do
    allTimes :: [Entity Times] <- selectList [TimesMeeting ==. entityKey meet] []
    parsedTime <- mapM buildTimes' $ map entityVal allTimes
    return $ Tables.MeetTime {meetData = entityVal meet, timeData = parsedTime}

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
courseInfo dept = fmap createJSONResponse (getDeptCourses dept)

getDeptCourses :: MonadIO m => T.Text -> m [Course]
getDeptCourses dept =
    liftIO $ runSqlite databasePath $ do
        courses  :: [Entity Courses]  <- selectList [] []
        meetings :: [Entity Meeting]  <- selectList [] []
        allMeetTimes <- mapM buildMeetTimes meetings
        let c = filter (T.isPrefixOf dept . coursesCode) $ map entityVal courses
        mapM (buildCourse allMeetTimes) c

-- | Return a list of all departments.
deptList :: IO Response
deptList = do
    depts <- runSqlite databasePath $ do
        courses :: [Entity Courses] <- selectList [] []
        return $ sort . nub $ map g courses :: SqlPersistM [String]
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
getMeetingTime :: (T.Text, T.Text, T.Text) -> SqlPersistM [Times']
getMeetingTime (meetingCode_, meetingSection_, meetingSession_) = do
    maybeEntityMeetings <- selectFirst [MeetingCode ==. meetingCode_,
                                        MeetingSection ==. getMeetingSection meetingSection_,
                                        MeetingSession ==. meetingSession_]
                                       []
    allTimes <- selectList [TimesMeeting ==. fromJust (fmap entityKey maybeEntityMeetings)] []
    parsedTime <- mapM buildTimes' $ map entityVal allTimes
    return parsedTime

getMeetingSection :: T.Text -> T.Text
getMeetingSection sec
    | T.isPrefixOf "L" sec = T.append "LEC" sectCode
    | T.isPrefixOf "T" sec = T.append "TUT" sectCode
    | T.isPrefixOf "P" sec = T.append "PRA" sectCode
    | otherwise            = sec
    where sectCode = T.tail sec
