module RequirementTests.CourseControllerTests
( courseContTestSuite ) where

import Config (runDb)
import Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Test.HUnit
import Happstack.Server (HttpVersion(..), Request(..), simpleHTTP'', rsBody, Response, ServerPart, Method(GET))
import Control.Concurrent.MVar (newMVar, newEmptyMVar)
import Database.Database (clearDatabase)
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables
import Controllers.Course (index)

-- | A minimal mock request for running a ServerPart
mockRequest :: IO Request
mockRequest = do
    inputsBody <- newMVar []
    requestBody <- newEmptyMVar
    return Request
        { rqSecure          = False             
        , rqMethod          = GET               
        , rqPaths           = []                
        , rqUri             = "/"               
        , rqQuery           = ""                
        , rqInputsQuery     = []
        , rqInputsBody      = inputsBody
        , rqCookies         = []                
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = requestBody
        , rqPeer            = ("127.0.0.1", 0)  
        }

-- | Helper function to run ServerPart Response
runServerPart :: ServerPart Response -> IO Response
runServerPart sp = do
    request <- mockRequest
    simpleHTTP'' sp request

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
        runDb clearDatabase
        runDb $ insertCourses courses
        response <- runServerPart Controllers.Course.index
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Run all the index test cases
runIndexTests :: [Test]
runIndexTests = Prelude.map (\(label, courses, expected) -> runIndexTest label courses expected) indexTestCases

-- | Test suite for Course Controller Module
courseContTestSuite :: IO Test
courseContTestSuite = do
    let tests = TestLabel "Course Controller tests" $ TestList runIndexTests
    return tests
