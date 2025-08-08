{-|
Description: Helper functions for various controller modules tests.

Module that contains helper functions used in testing controller module functions.

-}

module TestHelpers
    (acquireDatabase,
    mockRequest,
    runServerPart,
    clearDatabase,
    releaseDatabase,
    runServerPartWithQuery,
    runServerPartWithCourseInfoQuery,
    runServerPartWithGraphGenerate,
    withDatabase)
    where

import Config (databasePath)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map as Map
import Data.Text (unpack)
import Database.Database (setupDatabase)
import Database.Persist.Sqlite (Filter, SqlPersistM, deleteWhere)
import Database.Tables
import Happstack.Server (ContentType (..), HttpVersion (..), Input (..), Method (GET, PUT),
                         Request (..), Response, RqBody (..), ServerPart, inputContentType,
                         inputFilename, inputValue, simpleHTTP'')
import System.Directory (removeFile)
import System.Environment (setEnv, unsetEnv)
import Test.Tasty (TestTree, testGroup, withResource)

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

-- | A mock request for running ServerPartWithQuery, specifically for retrieveCourse
mockRequestWithQuery :: String -> IO Request
mockRequestWithQuery courseName = do
    inputsBody <- newMVar []
    requestBody <- newEmptyMVar
    return Request
        { rqSecure          = False
        , rqMethod          = GET
        , rqPaths           = ["course"]
        , rqUri             = "/course"
        , rqQuery           = ""
        , rqInputsQuery     = [("name", Input {
            inputValue = Right (BSL8.pack courseName),
            inputFilename = Nothing,
            inputContentType = defaultContentType
          })]
        , rqInputsBody      = inputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = requestBody
        , rqPeer            = ("127.0.0.1", 0)
        }

-- | A mock request for running ServerPartWithCourseInfoQuery, specifically for courseInfo
mockRequestWithCourseInfoQuery :: String -> IO Request
mockRequestWithCourseInfoQuery dept = do
    inputsBody <- newMVar []
    requestBody <- newEmptyMVar
    return Request
        { rqSecure          = False
        , rqMethod          = GET
        , rqPaths           = ["course-info"]
        , rqUri             = "/course-info"
        , rqQuery           = ""
        , rqInputsQuery     = [("dept", Input {
            inputValue = Right (BSL8.pack dept),
            inputFilename = Nothing,
            inputContentType = defaultContentType
          })]
        , rqInputsBody      = inputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = requestBody
        , rqPeer            = ("127.0.0.1", 0)
        }

-- | A mock request for the graph generate route, specifically for findAndSavePrereqsResponse
mockRequestWithGraphGenerate :: BSL.ByteString -> IO Request
mockRequestWithGraphGenerate payload = do
    inputsBody <- newMVar []
    requestBody <- newEmptyMVar
    putMVar requestBody (Body payload)

    return Request
        { rqSecure          = False
        , rqMethod          = PUT
        , rqPaths           = ["graph-generate"]
        , rqUri             = "/graph-generate"
        , rqQuery           = ""
        , rqInputsQuery     = []
        , rqInputsBody      = inputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = requestBody
        , rqPeer            = ("127.0.0.1", 0)
        }

-- | Default content type for the MockRequestWithQuery, specifically for retrieveCourse
defaultContentType :: ContentType
defaultContentType = ContentType
    { ctType = "text"
    , ctSubtype = "html"
    , ctParameters = []
    }

-- | Helper function to run ServerPartWithQuery Response
runServerPartWithQuery :: ServerPart Response -> String -> IO Response
runServerPartWithQuery sp courseName = do
    request <- mockRequestWithQuery courseName
    simpleHTTP'' sp request

-- | Helper function to run ServerPartWithQuery Response for courseInfo
runServerPartWithCourseInfoQuery :: ServerPart Response -> String -> IO Response
runServerPartWithCourseInfoQuery sp dept = do
    request <- mockRequestWithCourseInfoQuery dept
    simpleHTTP'' sp request

-- | Helper function to run ServerPartWithGraphGenerate for findAndSavePrereqsResponse
runServerPartWithGraphGenerate :: ServerPart Response -> BSL.ByteString -> IO Response
runServerPartWithGraphGenerate sp payload = do
    request <- mockRequestWithGraphGenerate payload
    simpleHTTP'' sp request

-- | Clear all the entries in the database
clearDatabase :: SqlPersistM ()
clearDatabase = do
    deleteWhere ([] :: [Filter Department])
    deleteWhere ([] :: [Filter Courses])
    deleteWhere ([] :: [Filter Meeting])
    deleteWhere ([] :: [Filter Times])
    deleteWhere ([] :: [Filter Breadth])
    deleteWhere ([] :: [Filter Distribution])
    deleteWhere ([] :: [Filter Graph])
    deleteWhere ([] :: [Filter Database.Tables.Text])
    deleteWhere ([] :: [Filter Shape])
    deleteWhere ([] :: [Filter Path])
    deleteWhere ([] :: [Filter Post])
    deleteWhere ([] :: [Filter PostCategory])
    deleteWhere ([] :: [Filter Building])

acquireDatabase :: IO ()
acquireDatabase = do
    setEnv "APP_ENV" "test"
    setupDatabase True

releaseDatabase :: () -> IO ()
releaseDatabase _ = do
    path <- databasePath
    removeFile $ unpack path
    unsetEnv "APP_ENV"

withDatabase ::  String -> [TestTree] -> TestTree
withDatabase label tests =
    withResource acquireDatabase releaseDatabase $ \_ ->
    testGroup label tests
