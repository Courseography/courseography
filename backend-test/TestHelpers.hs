{-|
Description: Helper functions for various controller modules tests.

Module that contains helper functions used in testing controller module functions.

-}

module TestHelpers
    (acquireDatabase,
    mockGetRequest,
    mockPutRequest,
    runServerPartWith,
    runServerPart,
    clearDatabase,
    releaseDatabase,
    withDatabase)
    where

import Config (databasePath)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar)
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List.Split (splitOn)
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

-- | Generalized function to create a mock request
createMockRequest :: Method -> String -> [(String, String)] -> BSL8.ByteString -> IO Request
createMockRequest reqMethod reqUri queryInputs body = do
    reqInputsBody <- newMVar []
    reqBody <- newEmptyMVar

    -- If a payload is provided, write it into the request body
    when (body /= "") $
        putMVar reqBody (Body body)
    return Request
        { rqSecure          = False
        , rqMethod          = reqMethod
        , rqPaths           = splitPath reqUri
        , rqUri             = reqUri
        , rqQuery           = ""
        , rqInputsQuery     = map (fmap convertInput) queryInputs
        , rqInputsBody      = reqInputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = reqBody
        , rqPeer            = ("127.0.0.1", 0)
        }
  where
    -- | Helper to convert a String to a query input parameter
    convertInput :: String -> Input
    convertInput paramValue = Input
        { inputValue = Right (BSL8.pack paramValue)
        , inputFilename = Nothing
        , inputContentType = defaultContentType
        }

    -- | Split a URI into path segments
    splitPath :: String -> [String]
    splitPath = splitOn "/" uri

-- | Curried version of createMockRequest for GET requests
mockGetRequest :: String -> [(String, String)] -> BSL8.ByteString -> IO Request
mockGetRequest = createMockRequest GET

-- | Curried version of createMockRequest for PUT requests
mockPutRequest :: String -> [(String, String)] -> BSL8.ByteString -> IO Request
mockPutRequest = createMockRequest PUT

-- | Default content type for the MockRequestWithQuery, specifically for retrieveCourse
defaultContentType :: ContentType
defaultContentType = ContentType
    { ctType = "text"
    , ctSubtype = "html"
    , ctParameters = []
    }

-- | Run a 'ServerPart' with a custom request.
runServerPartWith :: ServerPart Response -> IO Request -> IO Response
runServerPartWith sp reqIO = simpleHTTP'' sp =<< reqIO

-- Run a 'ServerPart' with GET request for testing wihtout query parameters.
runServerPart :: ServerPart Response -> IO Response
runServerPart sp = runServerPartWith sp (mockGetRequest "/" [] "")

-- | Clear all the entries in the database
clearDatabase :: SqlPersistM ()
clearDatabase = do
    deleteWhere ([] :: [Filter Department])
    deleteWhere ([] :: [Filter Courses])
    deleteWhere ([] :: [Filter Meeting])
    deleteWhere ([] :: [Filter Times])
    deleteWhere ([] :: [Filter Breadth])
    deleteWhere ([] :: [Filter Distribution])
    deleteWhere ([] :: [Filter Database.Tables.Text])
    deleteWhere ([] :: [Filter Shape])
    deleteWhere ([] :: [Filter Path])
    deleteWhere ([] :: [Filter Program])
    deleteWhere ([] :: [Filter ProgramCategory])
    deleteWhere ([] :: [Filter Building])
    deleteWhere ([] :: [Filter Graph])

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
