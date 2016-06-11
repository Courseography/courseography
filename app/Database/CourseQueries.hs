{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-|
Description: Respond to various requests involving database course information.

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
     returnTutorial,
     returnLecture,
     getGraphJSON) where

import Happstack.Server.SimpleHTTP
import Database.Persist
import Database.Persist.Sqlite
import Database.Tables as Tables
import Control.Monad.IO.Class (liftIO, MonadIO)
import Util.Happstack (createJSONResponse)
import qualified Data.Text as T
import WebParsing.ParsingHelp
import Data.String.Utils
import Data.List
import Config (databasePath)
import Control.Monad (liftM)
import Data.Aeson ((.=), toJSON, object)
import Data.Int (Int64)
import Database.DataType
import Svg.Builder

-- ** Querying a single course

-- | Takes a course code (e.g. \"CSC108H1\") and sends a JSON representation
-- of the course as a response.
retrieveCourse :: String -> ServerPart Response
retrieveCourse = liftIO . queryCourse . T.pack

-- | Queries the database for all information about @course@, constructs a JSON object
-- representing the course and returns the appropriate JSON response.
queryCourse :: T.Text -> IO Response
queryCourse str = do
    courseJSON <- returnCourse str
    return $ createJSONResponse courseJSON

-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO Course
returnCourse lowerStr = runSqlite databasePath $ do
    let courseStr = T.toUpper lowerStr
    sqlCourse :: [Entity Courses] <- selectList [CoursesCode ==. courseStr] [Asc CoursesCode]
    -- TODO: Just make one query for all lectures, then partition later.
    -- Same for tutorials.
    sqlLecturesFall    :: [Entity Lecture]   <- selectList
        [LectureCode  ==. courseStr, LectureSession ==. "F"] [Asc LectureSection]
    sqlLecturesSpring  :: [Entity Lecture]   <- selectList
        [LectureCode  ==. courseStr, LectureSession ==. "S"] [Asc LectureSection]
    sqlLecturesYear    :: [Entity Lecture]   <- selectList
        [LectureCode  ==. courseStr, LectureSession ==. "Y"] [Asc LectureSection]
    sqlTutorialsFall   :: [Entity Tutorial]  <- selectList
        [TutorialCode ==. courseStr, TutorialSession ==. "F"] [Asc TutorialSection]
    sqlTutorialsSpring :: [Entity Tutorial]  <- selectList
        [TutorialCode ==. courseStr, TutorialSession ==. "S"] [Asc TutorialSection]
    sqlTutorialsYear   :: [Entity Tutorial]  <- selectList
        [TutorialCode ==. courseStr, TutorialSession ==. "Y"] [Asc TutorialSection]
    let fallSession   = buildSession sqlLecturesFall sqlTutorialsFall
        springSession = buildSession sqlLecturesSpring sqlTutorialsSpring
        yearSession   = buildSession sqlLecturesYear sqlTutorialsYear
    if null sqlCourse
    then return emptyCourse
    else return (buildCourse fallSession springSession yearSession (entityVal $ head sqlCourse))

-- | Queries the database for all information regarding a specific tutorial for
-- a @course@, returns a Tutorial.
returnTutorial :: T.Text -> T.Text -> T.Text -> IO (Maybe Tutorial)
returnTutorial lowerStr sect session = runSqlite databasePath $ do
    maybeEntityTutorials <- selectFirst [TutorialCode ==. T.toUpper lowerStr,
                                         TutorialSection ==. Just sect,
                                         TutorialSession ==. session]
                                        []
    return $ fmap entityVal maybeEntityTutorials

-- | Queries the database for all information regarding a specific lecture for
--  a @course@, returns a Lecture.
returnLecture :: T.Text -> T.Text -> T.Text -> IO (Maybe Lecture)
returnLecture lowerStr sect session = runSqlite databasePath $ do
    maybeEntityLectures <- selectFirst [LectureCode ==. T.toUpper lowerStr,
                                        LectureSection ==. sect,
                                        LectureSession ==. session]
                                       []
    return $ fmap entityVal maybeEntityLectures

-- | Builds a Course structure from a tuple from the Courses table.
-- Some fields still need to be added in.
buildCourse :: Maybe Session -> Maybe Session -> Maybe Session -> Courses -> Course
buildCourse fallSession springSession yearSession course =
    Course (coursesBreadth course)
           (coursesDescription course)
           (coursesTitle course)
           (coursesPrereqString course)
           fallSession
           springSession
           yearSession
           (coursesCode course)
           (coursesExclusions course)
           (coursesManualTutorialEnrolment course)
           (coursesManualPracticalEnrolment course)
           (coursesDistribution course)
           (coursesPrereqs course)
           (coursesCoreqs course)
           (coursesVideoUrls course)

-- | Builds a Session structure from a list of tuples from the Lecture table,
-- and a list of tuples from the Tutorial table.
buildSession :: [Entity Lecture] -> [Entity Tutorial] -> Maybe Tables.Session
buildSession lecs tuts =
    Just $ Tables.Session (map entityVal lecs)
                          (map entityVal tuts)

-- ** Other queries

-- | Looks up a graph using its title then gets the Shape, Text and Path elements
-- for rendering graph (returned as JSON).
getGraphJSON :: String -> IO Response
getGraphJSON graphName =
    runSqlite databasePath $ do
        graphEnt :: (Maybe (Entity Graph)) <- selectFirst [GraphTitle ==. graphName] []
        case graphEnt of
            Nothing -> return $ createJSONResponse ["texts" .= ([] :: [Text]),
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

                    texts          = map entityVal sqlTexts
                    rects          = zipWith (buildRect texts)
                                             (map entityVal sqlRects)
                                             (map keyAsInt sqlRects)
                    ellipses       = zipWith (buildEllipses texts)
                                             (map entityVal sqlEllipses)
                                             (map keyAsInt sqlEllipses)
                    paths          = zipWith (buildPath rects ellipses)
                                             (map entityVal sqlPaths)
                                             (map keyAsInt sqlPaths)
                    (regions, edges) = partition pathIsRegion paths
                    regionTexts    = filter (not .
                                             intersectsWithShape (rects ++ ellipses))
                                            texts

                    result = createJSONResponse $
                        object [
                            ("texts", toJSON $ texts ++ regionTexts),
                            ("shapes", toJSON $ rects ++ ellipses),
                            ("paths", toJSON $ paths ++ regions),
                            ("width", toJSON $ graphWidth $ entityVal graph),
                            ("height", toJSON $ graphHeight $ entityVal graph)
                        ]

                return result

-- | Builds a list of all course codes in the database.
allCourses :: IO Response
allCourses = do
  response <- runSqlite databasePath $ do
      courses :: [Entity Courses] <- selectList [] []
      let codes = map (coursesCode . entityVal) courses
      return $ T.unlines codes
  return $ toResponse response

-- | Returns all course info for a given department.
courseInfo :: String -> ServerPart Response
courseInfo dept = liftM createJSONResponse (getDeptCourses dept)

-- | Returns all course info for a given department.
getDeptCourses :: MonadIO m => String -> m [Course]
getDeptCourses dept =
    liftIO $ runSqlite databasePath $ do
        courses :: [Entity Courses]   <- selectList [] []
        lecs    :: [Entity Lecture]  <- selectList [] []
        tuts    :: [Entity Tutorial] <- selectList [] []
        let c = filter (startswith dept . T.unpack . coursesCode) $ map entityVal courses
        return $ map (buildTimes (map entityVal lecs) (map entityVal tuts)) c
    where
        lecByCode course = filter (\lec -> lectureCode lec == coursesCode course)
        tutByCode course = filter (\tut -> tutorialCode tut == coursesCode course)
        buildTimes lecs tuts course =
            let fallLectures = filter (\lec -> lectureSession lec == "F") lecs
                springLectures = filter (\lec -> lectureSession lec == "S") lecs
                yearLectures = filter (\lec -> lectureSession lec == "Y") lecs
                fallTutorials = filter (\tut -> tutorialSession tut == "F") tuts
                springTutorials = filter (\tut -> tutorialSession tut == "S") tuts
                yearTutorials = filter (\tut -> tutorialSession tut == "Y") tuts
                fallSession   = buildSession' (lecByCode course fallLectures) (tutByCode course fallTutorials)
                springSession = buildSession' (lecByCode course springLectures) (tutByCode course springTutorials)
                yearSession   = buildSession' (lecByCode course yearLectures) (tutByCode course yearTutorials)
            in
                buildCourse fallSession springSession yearSession course
        buildSession' lecs tuts =
            Just $ Tables.Session lecs
                                  tuts

-- | Return a list of all departments.
deptList :: IO Response
deptList = do
    depts <- runSqlite databasePath $ do
        courses :: [Entity Courses] <- selectList [] []
        return $ sort . nub $ map g courses
    return $ createJSONResponse depts
    where
        g = take 3 . T.unpack . coursesCode . entityVal

-- | Queries the graphs table and returns a JSON response of Graph JSON
-- objects.
queryGraphs :: IO Response
queryGraphs =
    runSqlite databasePath $
        do graphs :: [Entity Graph] <- selectList [] []
           return $ createJSONResponse graphs
