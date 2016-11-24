{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}

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
     getGraphJSON,
     getLectureTime,
     getTutorialTime) where

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
import Database.DataType
import Svg.Builder


---- | Queries db for all matching records with lecture or tutorial code of this course
lectureQuery :: T.Text -> SqlPersistM [Entity Lecture]
lectureQuery courseCode = selectList [LectureCode ==. courseCode] []

tutorialQuery :: T.Text -> SqlPersistM [Entity Tutorial]
tutorialQuery courseCode = selectList [TutorialCode ==. courseCode] []

splitSessionsT :: [Entity Tutorial] -> ([Entity Tutorial], [Entity Tutorial], [Entity Tutorial])
splitSessionsT tutorialsList =
    let fallTut = filter (\tut -> tutorialSession (entityVal tut) == "F") tutorialsList
        springTut = filter (\tut -> tutorialSession (entityVal tut) == "S") tutorialsList
        yearTut = filter (\tut -> tutorialSession (entityVal tut) == "Y") tutorialsList
    in  (fallTut, springTut, yearTut)

splitSessionsL :: [Entity Lecture] -> ([Entity Lecture], [Entity Lecture], [Entity Lecture])
splitSessionsL lecturesList =
    let fallLec = filter (\lec -> lectureSession (entityVal lec) == "F") lecturesList
        springLec = filter (\lec -> lectureSession (entityVal lec) == "S") lecturesList
        yearLec = filter (\lec -> lectureSession (entityVal lec) == "Y") lecturesList
    in (fallLec, springLec, yearLec)

-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO Course
returnCourse lowerStr = runSqlite databasePath $ do
    let courseStr = T.toUpper lowerStr
    sqlCourse :: (Maybe (Entity Courses)) <- selectFirst [CoursesCode ==. courseStr] []
    case sqlCourse of
      Nothing -> return emptyCourse
      Just course -> do
        lecturesList :: [Entity Lecture] <- lectureQuery courseStr
        tutorialsList :: [Entity Tutorial] <- tutorialQuery courseStr
        let (fall, spring, year) = buildAllSessions lecturesList tutorialsList
        buildCourse fall spring year (entityVal course)

buildAllSessions :: [Entity Lecture] -> [Entity Tutorial] -> (Maybe Tables.Session, Maybe Tables.Session, Maybe Tables.Session)
buildAllSessions entityListL entityListT =
    let (fallLec, springLec, yearLec) = splitSessionsL entityListL
        (fallTut, springTut, yearTut) = splitSessionsT entityListT
        fall = buildSession fallLec fallTut
        spring = buildSession springLec springTut
        year = buildSession yearLec yearTut
    in (fall, spring, year)

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
        Just coursebreadth  -> return $ Just $ T.pack (breadthDescription coursebreadth)

getDescriptionD :: Maybe (Key Distribution) -> SqlPersistM (Maybe T.Text)
getDescriptionD Nothing = return Nothing
getDescriptionD (Just key) = do
    maybeDistribution <- get key
    case maybeDistribution of
        Nothing -> return Nothing
        Just dist -> return $ Just $ T.pack (distributionDescription dist)

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

                return response

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
        mapM (buildTimes (map entityVal lecs) (map entityVal tuts)) c
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
                fall   = buildSession' (lecByCode course fallLectures) (tutByCode course fallTutorials)
                spring = buildSession' (lecByCode course springLectures) (tutByCode course springTutorials)
                year   = buildSession' (lecByCode course yearLectures) (tutByCode course yearTutorials)
            in
                buildCourse fall spring year course
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
        do graphs :: [Entity Graph] <- selectList [] [Asc GraphTitle]
           return $ createJSONResponse graphs


getLectureTime :: CourseInfo -> SqlPersistM CourseInfo
getLectureTime courseInfo = do
    maybeEntityLectures <- selectFirst [LectureCode ==. (T.pack $ code courseInfo),
                                        LectureSection ==. (T.pack $ section courseInfo),
                                        LectureSession ==. (T.pack $ session courseInfo)]
                                       []
    let times = maybe [] (lectureTimes . entityVal) maybeEntityLectures
    return courseInfo { time = times }


getTutorialTime :: CourseInfo -> SqlPersistM CourseInfo
getTutorialTime courseInfo = do
    maybeEntityTutorials <- selectFirst [TutorialCode ==. (T.pack $ code courseInfo),
                                         TutorialSection ==. (Just (T.pack $ section courseInfo)),
                                         TutorialSession ==. (T.pack $ session courseInfo)]
                                        []
    let times = maybe [] (tutorialTimes . entityVal) maybeEntityTutorials
    return courseInfo { time = times }

