{-|
Description: Course Controller module tests.

Module that contains the tests for the functions in the Course Controller module.

-}

module Controllers.CourseControllerTests
( courseControllerTestSuite
) where

import Config (runDb)
import Control.Monad (unless)
import Controllers.Course (index, retrieveCourse)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Courses(..))
import Happstack.Server (rsBody)
import Test.HUnit (Test(..), assertEqual)
import TestHelpers (clearDatabase, runServerPart, runServerPartWithQuery)

-- | List of test cases as (input course name, course data, expected JSON output)
retrieveCourseTestCases :: [(String, T.Text, Map.Map T.Text T.Text, String)]
retrieveCourseTestCases =
    [ ("Course exists", 
       "STA238", 
       Map.fromList [
        ("name", "STA238H1"),
        ("title", "Probability, Statistics and Data Analysis II"),
        ("description", "An introduction to statistical inference and practice. Statistical models and parameters, estimators of parameters and their statistical properties, methods of estimation, confidence intervals, hypothesis testing, likelihood function, the linear model. Use of statistical computation for data analysis and simulation."),
        ("prereqs", "STA237H1/  STA247H1/  STA257H1/  STAB52H3/  STA256H5"),
        ("exclusions", "ECO220Y1/  ECO227Y1/  GGR270H1/  PSY201H1/  SOC300H1/  SOC202H1/  SOC252H1/  STA220H1/  STA221H1/  STA255H1/  STA248H1/  STA261H1/  STA288H1/  EEB225H1/  STAB22H3/  STAB27H3/  STAB57H3/  STA220H5/  STA221H5/  STA258H5/  STA260H5/  ECO220Y5/  ECO227Y5"),
        ("breadth", "The Physical and Mathematical Universes (5)"), 
        ("distribution", "null"),
        ("prereqString", "STA237H1/  STA247H1/  STA257H1/  STAB52H3/  STA256H5"),
        ("coreqs", "CSC108H1/  CSC110Y1/  CSC148H1 *Note: the corequisite may be completed either concurrently or in advance."),
        ("videoUrls", "https://example.com/video1, https://example.com/video2")
        ],
        "{\"allMeetingTimes\":[],\"breadth\":null,\"coreqs\":\"CSC108H1/  CSC110Y1/  CSC148H1 *Note: the corequisite may be completed either concurrently or in advance.\",\"description\":\"An introduction to statistical inference and practice. Statistical models and parameters, estimators of parameters and their statistical properties, methods of estimation, confidence intervals, hypothesis testing, likelihood function, the linear model. Use of statistical computation for data analysis and simulation.\",\"distribution\":null,\"exclusions\":\"ECO220Y1/  ECO227Y1/  GGR270H1/  PSY201H1/  SOC300H1/  SOC202H1/  SOC252H1/  STA220H1/  STA221H1/  STA255H1/  STA248H1/  STA261H1/  STA288H1/  EEB225H1/  STAB22H3/  STAB27H3/  STAB57H3/  STA220H5/  STA221H5/  STA258H5/  STA260H5/  ECO220Y5/  ECO227Y5\",\"name\":\"STA238H1\",\"prereqString\":\"STA237H1/  STA247H1/  STA257H1/  STAB52H3/  STA256H5\",\"title\":\"Probability, Statistics and Data Analysis II\",\"videoUrls\":[\"https://example.com/video1\",\"https://example.com/video2\"]}"
    ),
    
    ("Course does not exist", 
       "STA238", 
       Map.empty, 
       "null"
    ),
    
    ("No course provided", 
       "", 
       Map.empty, 
       "null"
    )
    ]

-- | Run a test case (case, input, expected output) on the retrieveCourse function.
runRetrieveCourseTest :: String -> T.Text -> Map.Map T.Text T.Text -> String -> Test
runRetrieveCourseTest label courseName courseData expected =
    TestLabel label $ TestCase $ do
        let currCourseName = fromMaybe "" $ Map.lookup "name" courseData
        
        let videoUrls = case Map.lookup "videoUrls" courseData of
                Just urlsText -> map T.strip (T.splitOn "," urlsText)
                Nothing -> []

        let courseToInsert = 
                Courses
                    { coursesCode = currCourseName
                    , coursesTitle = Map.lookup "title" courseData
                    , coursesDescription = Map.lookup "description" courseData
                    , coursesPrereqs = Map.lookup "prereqs" courseData
                    , coursesExclusions = Map.lookup "exclusions" courseData
                    , coursesBreadth = Nothing
                    , coursesDistribution = Nothing
                    , coursesPrereqString = Map.lookup "prereqString" courseData
                    , coursesCoreqs = Map.lookup "coreqs" courseData
                    , coursesVideoUrls = videoUrls
                    }

        runDb $ do
            clearDatabase
            unless (T.null currCourseName) $
                insert_ courseToInsert

        response <- runServerPartWithQuery Controllers.Course.retrieveCourse (T.unpack courseName)
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the retrieveCourse test cases
runRetrieveCourseTests :: [Test]
runRetrieveCourseTests = map (\(label, courseName, courseData, expected) -> runRetrieveCourseTest label courseName courseData expected) retrieveCourseTestCases

-- | Helper function to insert courses into the database
insertCourses :: [T.Text] -> SqlPersistM ()
insertCourses = mapM_ insertCourse
    where
        insertCourse code = insert_ (Courses code Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [])

-- | List of test cases as (input courses, label, expected output)
indexTestCases :: [(String, [T.Text], String)]
indexTestCases =
    [ ("Empty database", [], "")
    , ("One course", ["CSC199"], "CSC199\n")
    , ("Multiple courses", ["CSC101", "CSC102", "CSC103", "CSC104", "CSC105"],
       "CSC101\nCSC102\nCSC103\nCSC104\nCSC105\n")
    ]

-- | Run a test case (case, input, expected output) on the index function.
runIndexTest :: String -> [T.Text] -> String -> Test
runIndexTest label courses expected =
    TestLabel label $ TestCase $ do
        runDb $ do
            clearDatabase
            insertCourses courses
        response <- runServerPart Controllers.Course.index
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the index test cases
runIndexTests :: [Test]
runIndexTests = map (\(label, courses, expected) -> runIndexTest label courses expected) indexTestCases

-- | Test suite for Course Controller Module
courseControllerTestSuite :: Test
courseControllerTestSuite = TestLabel "Course Controller tests" $ TestList (runRetrieveCourseTests ++ runIndexTests)
