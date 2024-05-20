{-|
    Module: Database.CourseQueries
    Description: Respond to various requests involving database course
                 information.

This module contains the functions that perform different database queries
and serve the information back to the client.
-}

module Database.CourseQueries
    (retrieveCourse,
     retrievePost,
     returnCourse,
     allCourses,
     prereqsForCourse,
     courseInfo,
     getDeptCourses,
     depts,
     returnMeeting,
     getGraph,
     getGraphJSON,
     getMeetingTime,
     buildTime) where

import Config (databasePath)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (object, toJSON, (.=))
import Data.List
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Database.DataType
import Database.Persist
import Database.Persist.Sqlite
import Database.Tables as Tables
import Happstack.Server.SimpleHTTP
import Svg.Builder
import Util.Happstack (createJSONResponse)

-- | Queries the database for all matching lectures, tutorials,
meetingQuery :: [T.Text] -> SqlPersistM [MeetTime']
meetingQuery meetingCodes = do
    allMeetings <- selectList [MeetingCode <-. meetingCodes] []
    mapM buildMeetTimes allMeetings

-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO (Maybe Course)
returnCourse lowerStr = runSqlite databasePath $ do
    let courseStr = T.toUpper lowerStr
    -- TODO: require the client to pass the full course code
    let fullCodes = [courseStr, T.append courseStr "H1", T.append courseStr "Y1"]
    sqlCourse :: (Maybe (Entity Courses)) <- selectFirst [CoursesCode <-. fullCodes] []
    case sqlCourse of
      Nothing -> return Nothing
      Just course -> do
        meetings <- meetingQuery fullCodes
        Just <$> buildCourse meetings
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
returnPost code = runSqlite databasePath $ do
    sqlPost <- selectFirst [PostCode ==. code] []
    case sqlPost of
        Nothing -> return Nothing
        Just post -> return $ Just $ entityVal post

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

-- | Looks up a graph using its title then gets the Shape, Text and Path elements
-- for rendering graph (returned as JSON).
getGraphJSON :: T.Text -> IO Response
getGraphJSON graphName = getGraph graphName >>= withDefault
    where
        withDefault (Just response) = return response
        withDefault Nothing = return $
            createJSONResponse $
            object ["texts" .= ([] :: [Text]),
                    "shapes" .= ([] :: [Text]),
                    "paths" .= ([] :: [Text])]

getGraph :: T.Text -> IO (Maybe Response)
getGraph graphName =
    runSqlite databasePath $ do
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

                    response = createJSONResponse $
                        object [
                            ("texts", toJSON $ graphtexts ++ regionTexts),
                            ("shapes", toJSON $ rects ++ ellipses),
                            ("paths", toJSON $ graphpaths ++ regions),
                            ("width", toJSON $ graphWidth $ entityVal graph),
                            ("height", toJSON $ graphHeight $ entityVal graph)
                        ]

                return (Just response) :: SqlPersistM (Maybe Response)

-- | Builds a list of all course codes in the database.
allCourses :: IO Response
allCourses = do
  response <- runSqlite databasePath $ do
      courses :: [Entity Courses] <- selectList [] []
      let codes = map (coursesCode . entityVal) courses
      return $ T.unlines codes :: SqlPersistM T.Text
  return $ toResponse response

-- | Retrieves the prerequisites for a course (code) as a string.
prereqsForCourse :: T.Text -> IO (Either String T.Text)
prereqsForCourse courseCode = runSqlite databasePath $ do
    course <- selectFirst [CoursesCode ==. courseCode] []
    case course of
        Nothing -> return (Left "Course not found")
        Just courseEntity ->
            return (Right $
                fromMaybe "" $
                coursesPrereqString $
                entityVal courseEntity) :: SqlPersistM (Either String T.Text)

-- | Returns all course info for a given department.
courseInfo :: T.Text -> ServerPart Response
courseInfo dept = fmap createJSONResponse (getDeptCourses dept)

getDeptCourses :: MonadIO m => T.Text -> m [Course]
getDeptCourses dept =
    liftIO $ runSqlite databasePath $ do
        courses :: [Entity Courses] <- rawSql "SELECT ?? FROM courses WHERE code LIKE ?" [PersistText $ T.snoc dept '%']
        let deptCourses = map entityVal courses
        meetings :: [Entity Meeting] <- selectList [MeetingCode <-. map coursesCode deptCourses] []
        mapM (processCourse meetings) deptCourses
    where
        processCourse allMeetings course = do
            let courseMeetings = filter (\m -> meetingCode (entityVal m) == coursesCode course) allMeetings
            allTimes <- mapM buildMeetTimes courseMeetings
            buildCourse allTimes course

-- | Return a list of all departments.
depts :: IO Response
depts = do
    deptsList <- runSqlite databasePath $ do
        courses :: [Entity Courses] <- selectList [] []
        return $ sort . nub $ map g courses :: SqlPersistM [String]
    return $ createJSONResponse deptsList
    where
        g = take 3 . T.unpack . coursesCode . entityVal


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
