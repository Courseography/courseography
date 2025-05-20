{-|
Description: Course Controller module tests.

Module that contains the tests for the functions in the Course Controller module.

-}

module Controllers.CourseControllerTests
( courseControllerTestSuite
) where

import Config (runDb)
import Control.Monad (unless, when)
import Controllers.Course (courseInfo, depts, index, retrieveCourse)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Courses(..))
import Happstack.Server (rsBody)
import Test.HUnit (Test(..), assertEqual)
import TestHelpers (clearDatabase, runServerPart, runServerPartWithQuery, runServerPartWithCourseInfoQuery)

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

-- | List of test cases as (label, input courses, expected output)
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

-- | List of dept test cases; formatted as (test label, input db courses, expected output)
deptsTestCases :: [(String, [T.Text], String)]
deptsTestCases =
    [
        ("empty db", [], "[]"),
        ("one course", ["MAT137"], "[\"MAT\"]"),
        ("multiple, diff depts", ["STA237", "CSC236", "MAT237"], "[\"CSC\",\"MAT\",\"STA\"]"),
        ("multiple, same dept", ["CSC110", "CSC111", "CSC108"], "[\"CSC\"]")
    ]

-- | Run a test case (args: case description/label, input, expected output) on the depts function
runDeptsTest :: String -> [T.Text] -> String -> Test
runDeptsTest label courses expected =
    TestLabel label $ TestCase $ do
        runDb $ do
            clearDatabase
            insertCourses courses
        response <- runServerPart Controllers.Course.depts
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected output for test: " ++ label) expected actual

-- | Run all test cases for depts
runDeptsTests :: [Test]
runDeptsTests = map (\(label, courses, expected) -> runDeptsTest label courses expected) deptsTestCases

-- | List of test cases as (case, database state, input [dept], expected JSON output) for the courseInfo function
courseInfoTestCases :: [(String, Int, T.Text, String)]
courseInfoTestCases = 
    [ ("Empty Database"
    , 0
    , "STA"
    , "[]")
    , ("Department with one course in database called"
    , 1
    , "CSC"
    , "[{\"allMeetingTimes\":[],\"breadth\":null,\"coreqs\":null,\"description\":\"Programming in a language such as Python. Elementary data types, lists, maps. Program structure: control flow, functions, classes, objects, methods. Algorithms and problem solving. Searching, sorting, and complexity. Unit testing. Floating-point numbers and numerical computation. No prior programming experience required.\",\"distribution\":null,\"exclusions\":\"CSC110Y1,  CSC111H1,  CSC120H1,  CSC121H1,  CSC148H1,  CSC108H5,  CSC148H5,  CSCA08H3,  CSCA20H3,  CSCA48H3\",\"name\":\"CSC108H1\",\"prereqString\":null,\"title\":\"Introduction to Computer Programming\",\"videoUrls\":[]}]")
    , ("Department with two courses in database called"
    , 1
    , "STA"
    , "[{\"allMeetingTimes\":[],\"breadth\":null,\"coreqs\":\"(  CSC108H1/  equivalent programming experience)/  CSC110Y1/  CSC148H1 *Note: the corequisite may be completed either concurrently or in advance.\",\"description\":\"An introduction to probability using simulation and mathematical frameworks, with emphasis on the probability needed for more advanced study in statistical practice. Topics covered include probability spaces, random variables, discrete and continuous probability distributions, probability mass, density, and distribution functions, expectation and variance, independence, conditional probability, the law of large numbers, the central limit theorem, sampling distributions. Computer simulation will be taught and used extensively for calculations and to guide the theoretical development.\",\"distribution\":null,\"exclusions\":\"STA247H1,  STA201H1,  STA255H1,  STA257H1,  ECO227Y1,  MAT370H1,  STAB52H3,  STA256H5,  ECO227Y5\",\"name\":\"STA237H1\",\"prereqString\":\"(  MAT135H1,  MAT136H1)/  MAT137Y1/  MAT157Y1/  (  MATA30H3,  MATA36H3)/  (  MATA31H3,  MATA37H3)/  (  MAT135H5,  MAT136H5)/  MAT137Y5/  MAT157Y5/  ( MAT137H5,  MAT139H5)/  ( MAT157H5,  MAT159H5)\",\"title\":\"Probability, Statistics and Data Analysis I\",\"videoUrls\":[]},{\"allMeetingTimes\":[],\"breadth\":null,\"coreqs\":\"CSC108H1/  CSC110Y1/  CSC148H1 *Note: the corequisite may be completed either concurrently or in advance.\",\"description\":\"An introduction to statistical inference and practice. Statistical models and parameters, estimators of parameters and their statistical properties, methods of estimation, confidence intervals, hypothesis testing, likelihood function, the linear model. Use of statistical computation for data analysis and simulation.\",\"distribution\":null,\"exclusions\":\"ECO220Y1/  ECO227Y1/  GGR270H1/  PSY201H1/  SOC300H1/  SOC202H1/  SOC252H1/  STA220H1/  STA221H1/  STA255H1/  STA248H1/  STA261H1/  STA288H1/  EEB225H1/  STAB22H3/  STAB27H3/  STAB57H3/  STA220H5/  STA221H5/  STA258H5/  STA260H5/  ECO220Y5/  ECO227Y5\",\"name\":\"STA238H1\",\"prereqString\":\"STA237H1/  STA247H1/  STA257H1/  STAB52H3/  STA256H5\",\"title\":\"Probability, Statistics and Data Analysis II\",\"videoUrls\":[\"[https://example.com/video1\",\"https://example.com/video2]\"]}]")
    , ("Department with no courses in database called"
    , 1
    , "MAT"
    , "[]")
    , ("Empty Department Called In a Non - Empty Database -- should return the entire database"
    , 1
    , ""
    , "[{\"allMeetingTimes\":[],\"breadth\":null,\"coreqs\":\"(  CSC108H1/  equivalent programming experience)/  CSC110Y1/  CSC148H1 *Note: the corequisite may be completed either concurrently or in advance.\",\"description\":\"An introduction to probability using simulation and mathematical frameworks, with emphasis on the probability needed for more advanced study in statistical practice. Topics covered include probability spaces, random variables, discrete and continuous probability distributions, probability mass, density, and distribution functions, expectation and variance, independence, conditional probability, the law of large numbers, the central limit theorem, sampling distributions. Computer simulation will be taught and used extensively for calculations and to guide the theoretical development.\",\"distribution\":null,\"exclusions\":\"STA247H1,  STA201H1,  STA255H1,  STA257H1,  ECO227Y1,  MAT370H1,  STAB52H3,  STA256H5,  ECO227Y5\",\"name\":\"STA237H1\",\"prereqString\":\"(  MAT135H1,  MAT136H1)/  MAT137Y1/  MAT157Y1/  (  MATA30H3,  MATA36H3)/  (  MATA31H3,  MATA37H3)/  (  MAT135H5,  MAT136H5)/  MAT137Y5/  MAT157Y5/  ( MAT137H5,  MAT139H5)/  ( MAT157H5,  MAT159H5)\",\"title\":\"Probability, Statistics and Data Analysis I\",\"videoUrls\":[]},{\"allMeetingTimes\":[],\"breadth\":null,\"coreqs\":\"CSC108H1/  CSC110Y1/  CSC148H1 *Note: the corequisite may be completed either concurrently or in advance.\",\"description\":\"An introduction to statistical inference and practice. Statistical models and parameters, estimators of parameters and their statistical properties, methods of estimation, confidence intervals, hypothesis testing, likelihood function, the linear model. Use of statistical computation for data analysis and simulation.\",\"distribution\":null,\"exclusions\":\"ECO220Y1/  ECO227Y1/  GGR270H1/  PSY201H1/  SOC300H1/  SOC202H1/  SOC252H1/  STA220H1/  STA221H1/  STA255H1/  STA248H1/  STA261H1/  STA288H1/  EEB225H1/  STAB22H3/  STAB27H3/  STAB57H3/  STA220H5/  STA221H5/  STA258H5/  STA260H5/  ECO220Y5/  ECO227Y5\",\"name\":\"STA238H1\",\"prereqString\":\"STA237H1/  STA247H1/  STA257H1/  STAB52H3/  STA256H5\",\"title\":\"Probability, Statistics and Data Analysis II\",\"videoUrls\":[\"[https://example.com/video1\",\"https://example.com/video2]\"]},{\"allMeetingTimes\":[],\"breadth\":null,\"coreqs\":null,\"description\":\"Programming in a language such as Python. Elementary data types, lists, maps. Program structure: control flow, functions, classes, objects, methods. Algorithms and problem solving. Searching, sorting, and complexity. Unit testing. Floating-point numbers and numerical computation. No prior programming experience required.\",\"distribution\":null,\"exclusions\":\"CSC110Y1,  CSC111H1,  CSC120H1,  CSC121H1,  CSC148H1,  CSC108H5,  CSC148H5,  CSCA08H3,  CSCA20H3,  CSCA48H3\",\"name\":\"CSC108H1\",\"prereqString\":null,\"title\":\"Introduction to Computer Programming\",\"videoUrls\":[]}]")
    ]
    
-- | Run a test case (case, database state, input [dept], expected JSON output) on the courseInfo function
runCourseInfoTest :: String -> Int -> T.Text -> String -> Test
runCourseInfoTest label state dept expected =
    TestLabel label $ TestCase $ do

        let sta237 = Courses
                { coursesCode = "STA237H1"
                , coursesTitle = Just "Probability, Statistics and Data Analysis I"
                , coursesDescription = Just "An introduction to probability using simulation and mathematical frameworks, with emphasis on the probability needed for more advanced study in statistical practice. Topics covered include probability spaces, random variables, discrete and continuous probability distributions, probability mass, density, and distribution functions, expectation and variance, independence, conditional probability, the law of large numbers, the central limit theorem, sampling distributions. Computer simulation will be taught and used extensively for calculations and to guide the theoretical development."
                , coursesPrereqs = Just "(  MAT135H1,  MAT136H1)/  MAT137Y1/  MAT157Y1/  (  MATA30H3,  MATA36H3)/  (  MATA31H3,  MATA37H3)/  (  MAT135H5,  MAT136H5)/  MAT137Y5/  MAT157Y5/  ( MAT137H5,  MAT139H5)/  ( MAT157H5,  MAT159H5)"
                , coursesExclusions = Just "STA247H1,  STA201H1,  STA255H1,  STA257H1,  ECO227Y1,  MAT370H1,  STAB52H3,  STA256H5,  ECO227Y5"
                , coursesBreadth = Nothing
                , coursesDistribution = Nothing
                , coursesPrereqString = Just "(  MAT135H1,  MAT136H1)/  MAT137Y1/  MAT157Y1/  (  MATA30H3,  MATA36H3)/  (  MATA31H3,  MATA37H3)/  (  MAT135H5,  MAT136H5)/  MAT137Y5/  MAT157Y5/  ( MAT137H5,  MAT139H5)/  ( MAT157H5,  MAT159H5)"
                , coursesCoreqs = Just "(  CSC108H1/  equivalent programming experience)/  CSC110Y1/  CSC148H1 *Note: the corequisite may be completed either concurrently or in advance."
                , coursesVideoUrls = []
                }
            sta238 = Courses
                { coursesCode = "STA238H1"
                , coursesTitle = Just "Probability, Statistics and Data Analysis II"
                , coursesDescription = Just "An introduction to statistical inference and practice. Statistical models and parameters, estimators of parameters and their statistical properties, methods of estimation, confidence intervals, hypothesis testing, likelihood function, the linear model. Use of statistical computation for data analysis and simulation."
                , coursesPrereqs = Just "STA237H1/  STA247H1/  STA257H1/  STAB52H3/  STA256H5"
                , coursesExclusions = Just "ECO220Y1/  ECO227Y1/  GGR270H1/  PSY201H1/  SOC300H1/  SOC202H1/  SOC252H1/  STA220H1/  STA221H1/  STA255H1/  STA248H1/  STA261H1/  STA288H1/  EEB225H1/  STAB22H3/  STAB27H3/  STAB57H3/  STA220H5/  STA221H5/  STA258H5/  STA260H5/  ECO220Y5/  ECO227Y5"
                , coursesBreadth = Nothing
                , coursesDistribution = Nothing
                , coursesPrereqString = Just "STA237H1/  STA247H1/  STA257H1/  STAB52H3/  STA256H5"
                , coursesCoreqs = Just "CSC108H1/  CSC110Y1/  CSC148H1 *Note: the corequisite may be completed either concurrently or in advance."
                , coursesVideoUrls = [T.pack "[https://example.com/video1",T.pack "https://example.com/video2]"]
                }
            csc108 = Courses
                { coursesCode = "CSC108H1"
                , coursesTitle = Just "Introduction to Computer Programming"
                , coursesDescription = Just "Programming in a language such as Python. Elementary data types, lists, maps. Program structure: control flow, functions, classes, objects, methods. Algorithms and problem solving. Searching, sorting, and complexity. Unit testing. Floating-point numbers and numerical computation. No prior programming experience required."
                , coursesPrereqs = Nothing
                , coursesExclusions = Just "CSC110Y1,  CSC111H1,  CSC120H1,  CSC121H1,  CSC148H1,  CSC108H5,  CSC148H5,  CSCA08H3,  CSCA20H3,  CSCA48H3"
                , coursesBreadth = Nothing
                , coursesDistribution = Nothing
                , coursesPrereqString = Nothing
                , coursesCoreqs = Nothing
                , coursesVideoUrls = []
                }

        runDb $ do
            clearDatabase
            when (state == 1) $ do 
                insert_ sta237
                insert_ sta238
                insert_ csc108
        
        response <- runServerPartWithCourseInfoQuery Controllers.Course.courseInfo (T.unpack dept)
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all courseInfo test cases
runCourseInfoTests :: [Test]
runCourseInfoTests = map (\(label, state, dept, expected) -> runCourseInfoTest label state dept expected) courseInfoTestCases

-- | Test suite for Course Controller Module
courseControllerTestSuite :: Test
courseControllerTestSuite = TestLabel "Course Controller tests" $ TestList (runRetrieveCourseTests ++ runIndexTests ++ runDeptsTests ++ runCourseInfoTests)
