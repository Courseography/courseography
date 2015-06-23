{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-|
Description: Respond to various requests involving database course information.

This module contains the functions that perform different database queries
and serve the information back to the client.
-}

module Database.CourseQueries (retrieveCourse,
                               returnCourse,
                               allCourses,
                               courseInfo,
                               getDepartment,
                               queryGraphs,
                               deptList,
                               returnTutorialTimes,
                               returnLectureTimes) where

import Happstack.Server.SimpleHTTP
import Database.Persist
import Database.Persist.Sqlite
import Database.Tables as Tables
import Control.Monad.IO.Class (liftIO, MonadIO)
import JsonResponse
import qualified Data.Text as T
import WebParsing.ParsingHelp
import Data.String.Utils
import Data.List
import Config (dbStr)


-- ** Querying a single course

-- | Takes a course code (e.g. \"CSC108H1\") and sends a JSON representation
-- of the course as a response.
retrieveCourse :: String -> ServerPart Response
retrieveCourse course =
    liftIO $ queryCourse (T.pack course)

-- | Queries the database for all information about @course@, constructs a JSON object
-- representing the course and returns the appropriate JSON response.
queryCourse :: T.Text -> IO Response
queryCourse str = do
    courseJSON <- returnCourse str
    return $ createJSONResponse courseJSON

-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO Course
returnCourse lowerStr = runSqlite dbStr $ do
    let courseStr = T.toUpper lowerStr
    sqlCourse :: [Entity Courses] <- selectList [CoursesCode ==. courseStr] []
    -- TODO: Just make one query for all lectures, then partition later.
    -- Same for tutorials.
    sqlLecturesFall    :: [Entity Lectures]   <- selectList
        [LecturesCode  ==. courseStr, LecturesSession ==. "F"] []
    sqlLecturesSpring  :: [Entity Lectures]   <- selectList
        [LecturesCode  ==. courseStr, LecturesSession ==. "S"] []
    sqlLecturesYear    :: [Entity Lectures]   <- selectList
        [LecturesCode  ==. courseStr, LecturesSession ==. "Y"] []
    sqlTutorialsFall   :: [Entity Tutorials]  <- selectList
        [TutorialsCode ==. courseStr, TutorialsSession ==. "F"] []
    sqlTutorialsSpring :: [Entity Tutorials]  <- selectList
        [TutorialsCode ==. courseStr, TutorialsSession ==. "S"] []
    sqlTutorialsYear   :: [Entity Tutorials]  <- selectList
        [TutorialsCode ==. courseStr, TutorialsSession ==. "Y"] []
    let fallSession   = buildSession sqlLecturesFall sqlTutorialsFall
        springSession = buildSession sqlLecturesSpring sqlTutorialsSpring
        yearSession   = buildSession sqlLecturesYear sqlTutorialsYear
    if null sqlCourse
    then return emptyCourse
    else return (buildCourse fallSession springSession yearSession (entityVal $ head sqlCourse))
{-
returnTutorialTimes :: T.Text -> T.Text -> Maybe T.Text -> IO [Time]
returnTutorialTimes lowerStr session section = runSqlite dbStr $ do
    let courseStr = T.toUpper lowerStr
    entityTutorials <- selectList [TutorialsCode ==. courseStr, TutorialsSession ==. session, TutorialsSection ==. (section)] []
    return $ tutorialsTimes (entityVal $ head entityTutorials)


returnTutorialTimes :: T.Text -> T.Text -> T.Text -> IO (T.Text, [Time], T.Text)
returnTutorialTimes lowerStr section session = runSqlite dbStr $ do
    let courseStr = T.toUpper lowerStr
    entityTutorials <- selectList [TutorialsCode ==. courseStr, TutorialsSection ==. (Just section), TutorialsSession ==. session] []
    return $ (tutorialsTimeStr (entityVal $ head entityTutorials), tutorialsTimes (entityVal $ head entityTutorials), tutorialsCode (entityVal $ head entityTutorials))
    -}

returnTutorialTimes :: T.Text -> T.Text -> T.Text -> IO T.Text
returnTutorialTimes lowerStr section session = runSqlite dbStr $ do
    let courseStr = T.toUpper lowerStr
    entityTutorials <- selectList [TutorialsCode ==. courseStr, TutorialsSection ==. (Just section), TutorialsSession ==. session] []
    return $ tutorialsTimeStr (entityVal $ head entityTutorials)

returnLectureTimes :: T.Text -> T.Text -> T.Text -> IO T.Text
returnLectureTimes lowerStr section session = runSqlite dbStr $ do
    let courseStr = T.toUpper lowerStr
    entityLectures <- selectList [LecturesCode ==. courseStr, LecturesSection ==. section, LecturesSession ==. session] []
    return $ lecturesTimeStr (entityVal $ head entityLectures)

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

-- | Builds a Lecture structure from a tuple from the Lectures table.
buildLecture :: Lectures -> Lecture
buildLecture entity =
    Lecture (lecturesExtra entity)
            (lecturesSection entity)
            (lecturesCapacity entity)
            (lecturesTimeStr entity)
            (map timeField (lecturesTimes entity))
            (lecturesInstructor entity)
            (Just (lecturesEnrolled entity))
            (Just (lecturesWaitlist entity))

-- | Builds a Tutorial structure from a tuple from the Tutorials table.
buildTutorial :: Tutorials -> Tutorial
buildTutorial entity =
    Tutorial (tutorialsSection entity)
             (map timeField (tutorialsTimes entity))
             (tutorialsTimeStr entity)

-- | Builds a Session structure from a list of tuples from the Lectures table,
-- and a list of tuples from the Tutorials table.
buildSession :: [Entity Lectures] -> [Entity Tutorials] -> Maybe Tables.Session
buildSession lecs tuts =
    Just $ Tables.Session (map (buildLecture . entityVal) lecs)
                          (map (buildTutorial . entityVal) tuts)

-- ** Other queries

-- | Builds a list of all course codes in the database.
allCourses :: IO Response
allCourses = do
  response <- runSqlite dbStr $ do
      courses :: [Entity Courses] <- selectList [] []
      let codes = map (coursesCode . entityVal) courses
      return $ T.unlines codes
  return $ toResponse response

-- | Returns all course info for a given department.
courseInfo :: String -> ServerPart Response
courseInfo dept = do
      (getDeptCourses dept) >>=
        (\courses -> return $ createJSONResponse courses)

-- | Returns all courses for a given department.
getDepartment :: String -> IO [Course]
getDepartment str = getDeptCourses str

-- | Returns all course info for a given department.
getDeptCourses :: MonadIO m => String -> m [Course]
getDeptCourses dept = do
    response <- liftIO $ runSqlite dbStr $ do
        courses :: [Entity Courses]   <- selectList [] []
        lecs    :: [Entity Lectures]  <- selectList [] []
        tuts    :: [Entity Tutorials] <- selectList [] []
        let c = filter (startswith dept . T.unpack . coursesCode) $ map entityVal courses
        return $ map (buildTimes (map entityVal lecs) (map entityVal tuts)) c
    return response
    where
        lecByCode course = filter (\lec -> lecturesCode lec == coursesCode course)
        tutByCode course = filter (\tut -> tutorialsCode tut == coursesCode course)
        buildTimes lecs tuts course =
            let fallLectures = filter (\lec -> lecturesSession lec == "F") lecs
                springLectures = filter (\lec -> lecturesSession lec == "S") lecs
                yearLectures = filter (\lec -> lecturesSession lec == "Y") lecs
                fallTutorials = filter (\tut -> tutorialsSession tut == "F") tuts
                springTutorials = filter (\tut -> tutorialsSession tut == "S") tuts
                yearTutorials = filter (\tut -> tutorialsSession tut == "Y") tuts
                fallSession   = buildSession' (lecByCode course fallLectures) (tutByCode course fallTutorials)
                springSession = buildSession' (lecByCode course springLectures) (tutByCode course springTutorials)
                yearSession   = buildSession' (lecByCode course yearLectures) (tutByCode course yearTutorials)
            in
                buildCourse fallSession springSession yearSession course
        buildSession' lecs tuts =
            Just $ Tables.Session (map buildLecture lecs)
                                  (map buildTutorial tuts)

-- | Return a list of all departments.
deptList :: IO Response
deptList = do
    depts <- runSqlite dbStr $ do
        courses :: [Entity Courses] <- selectList [] []
        return $ sort . nub $ map g courses
    return $ createJSONResponse depts
    where
        g = take 3 . T.unpack . coursesCode . entityVal

-- | Queries the graphs table and returns a JSON response of Graph JSON
-- objects.
queryGraphs :: IO Response
queryGraphs =
    runSqlite dbStr $
        do graphs :: [Entity Graph] <- selectList [] []
           return $ createJSONResponse graphs
