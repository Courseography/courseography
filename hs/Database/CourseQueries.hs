{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Database.CourseQueries (retrieveCourse, allCourses, queryGraphs, courseInfo, deptList, returnCourse) where

import Database.Persist
import Database.Persist.Sqlite
import Database.Tables as Tables
import Control.Monad.IO.Class (liftIO)
import JsonResponse
import Database.JsonParser
import Happstack.Server
import Data.List
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import WebParsing.ParsingHelp
import Data.String.Utils

retrieveCourse :: String -> ServerPart Response
retrieveCourse course =
    liftIO $ queryCourse (T.pack course)


-- | Queries the database for all information about `course`, constructs a JSON object
-- | representing the course and returns the appropriate JSON response.
queryCourse :: T.Text -> IO Response
queryCourse str = do
  courseJSON <- returnCourse str
  return $ createJSONResponse courseJSON

-- | Queries the database for all information about `course`, constructs and returns a Course Record.
returnCourse :: T.Text -> IO Course
returnCourse lowerStr =
    runSqlite dbStr $ do
        let courseStr = T.toUpper lowerStr
        sqlCourse :: [Entity Courses] <- selectList [CoursesCode ==. courseStr] []
        sqlLecturesFall    :: [Entity Lectures]   <- selectList [LecturesCode  ==. courseStr,
                                                                 LecturesSession ==. "F"] []
        sqlLecturesSpring  :: [Entity Lectures]   <- selectList [LecturesCode  ==. courseStr,
                                                                 LecturesSession ==. "S"] []
        sqlLecturesYear    :: [Entity Lectures]   <- selectList [LecturesCode  ==. courseStr,
                                                                 LecturesSession ==. "Y"] []
        sqlTutorialsFall   :: [Entity Tutorials]  <- selectList [TutorialsCode ==. courseStr,
                                                                 TutorialsSession ==. "F"] []
        sqlTutorialsSpring :: [Entity Tutorials]  <- selectList [TutorialsCode ==. courseStr,
                                                                 TutorialsSession ==. "S"] []
        sqlTutorialsYear   :: [Entity Tutorials]  <- selectList [TutorialsCode ==. courseStr,
                                                                 TutorialsSession ==. "Y"] []
        
        if null sqlCourse
        then return emptyCourse
        else do
            let course        = entityVal $ head sqlCourse
                fallSession   = buildSession sqlLecturesFall sqlTutorialsFall
                springSession = buildSession sqlLecturesSpring sqlTutorialsSpring
                yearSession   = buildSession sqlLecturesYear sqlTutorialsYear
                courseJSON    = buildCourse fallSession springSession yearSession course
            return courseJSON

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

-- | Builds a Session structure from a list of tuples from the Lectures table, and a list of tuples from the Tutorials table.
buildSession :: [Entity Lectures] -> [Entity Tutorials] -> Maybe Tables.Session
buildSession lectures tutorials =
    Just $ Tables.Session (map (buildLecture . entityVal) lectures)
                          (map (buildTutorial . entityVal) tutorials)

-- | Build a list of all course codes in the database
allCourses :: IO Response
allCourses = do
  response <- runSqlite dbStr $ do
      courses :: [Entity Courses] <- selectList [] []
      let codes = map (coursesCode . entityVal) courses
      return $ T.unlines codes
  return $ toResponse response

-- | Return all course info for a given department
courseInfo :: String -> ServerPart Response
courseInfo dept = do
    response <- liftIO $ runSqlite dbStr $ do
        courses :: [Entity Courses] <- selectList [] []
        lecs    :: [Entity Lectures]   <- selectList [] []
        tuts  :: [Entity Tutorials]   <- selectList [] []
        let c = filter (startswith dept . T.unpack . coursesCode) $ map entityVal courses
        return $ map (buildTimes (map entityVal lecs) (map entityVal tuts)) c

    return $ createJSONResponse response
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
        buildSession' lectures tutorials =
            Just $ Tables.Session (map buildLecture lectures)
                                  (map buildTutorial tutorials)

-- | Return a list of all departments
deptList :: IO Response
deptList = do
    depts <- runSqlite dbStr $ do
        courses :: [Entity Courses] <- selectList [] []
        return $ sort . nub $ map f courses
    return $ createJSONResponse depts
    where
        f = take 3 . T.unpack . coursesCode . entityVal

-- | Queries the graphs table and returns a JSON response of Graph JSON
-- objects.
queryGraphs :: IO Response
queryGraphs =
    runSqlite dbStr $
        do graphs :: [Entity Graph] <- selectList [] []
           return $ createJSONResponse $ map entityVal graphs
