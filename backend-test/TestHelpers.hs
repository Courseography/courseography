{-|
Description: Helper functions for various controller modules tests.

Module that contains helper functions used in testing controller module functions.

-}

module TestHelpers
    (acquireDatabase,
    mockGetRequest,
    mockPutRequest,
    runServerPart,
    clearDatabase,
    releaseDatabase,
    runServerPartWithQuery,
    runServerPartWithCourseInfoQuery,
    runServerPartWithProgramQuery,
    runServerPartWithGraphGenerate,
    withDatabase)
    where

import Config (databasePath)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar)
import Control.Monad (when)
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

-- | Generalized function to create a mock request
createMockRequest :: Method -> String -> [(String, String)] -> Maybe BSL8.ByteString -> IO Request
createMockRequest reMethod reUri queryInputs maybeBody = do
    reInputsBody <- newMVar []
    reBody <- newEmptyMVar

    -- If a payload is provided, write it into the request body
    when (maybeBody /= Nothing) $
        putMVar reBody (Body (maybe BSL.empty id maybeBody))
    return Request
        { rqSecure          = False
        , rqMethod          = reMethod
        , rqPaths           = splitPath reUri
        , rqUri             = reUri
        , rqQuery           = ""
        , rqInputsQuery     = map (fmap convertInput) queryInputs
        , rqInputsBody      = reInputsBody
        , rqCookies         = []
        , rqVersion         = HttpVersion 1 1
        , rqHeaders         = Map.empty
        , rqBody            = reBody
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

    -- | Split a string when a predicate is true
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s = case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'

    -- | Split a URI into path segments
    splitPath :: String -> [String]
    splitPath uri = wordsWhen (== '/') uri

-- | Curried version of createMockRequest for GET requests
mockGetRequest :: String -> [(String, String)] -> IO Request
mockGetRequest reUri queryInputs = createMockRequest GET reUri queryInputs Nothing

-- | Curried version of createMockRequest for PUT requests
mockPutRequest :: String -> Maybe BSL8.ByteString -> IO Request
mockPutRequest reUri = createMockRequest PUT reUri []

-- | A minimal mock request for running a ServerPart
mockRequest :: IO Request
mockRequest = mockGetRequest "/" []

-- | A mock request with a query parameter for retrieveCourse
mockRequestWithQuery :: String -> IO Request
mockRequestWithQuery courseName = 
    mockGetRequest "/course" [("name", courseName)]

-- | A mock request with a query parameter for courseInfo
mockRequestWithCourseInfoQuery :: String -> IO Request
mockRequestWithCourseInfoQuery dept = 
    mockGetRequest "/course-info" [("dept", dept)]

-- | A mock request with a query parameter for retrieveProgram
mockRequestWithProgramQuery :: String -> IO Request
mockRequestWithProgramQuery programCode = 
    mockGetRequest "/program" [("code", programCode)]

-- | A mock request with a body payload for the graph generate route
mockRequestWithGraphGenerate :: BSL.ByteString -> IO Request
mockRequestWithGraphGenerate payload = 
    mockPutRequest "/graph-generate" (Just payload)

-- | Default content type for the MockRequestWithQuery, specifically for retrieveCourse
defaultContentType :: ContentType
defaultContentType = ContentType
    { ctType = "text"
    , ctSubtype = "html"
    , ctParameters = []
    }

-- | Generalized helper function to run a ServerPart with a request
runServerPartWith :: ServerPart Response -> IO Request -> IO Response
runServerPartWith sp requestIO = do
    request <- requestIO
    simpleHTTP'' sp request

-- | Helper function to run ServerPart Response
runServerPart :: ServerPart Response -> IO Response
runServerPart sp = runServerPartWith sp mockRequest

-- | Helper function to run ServerPart Response with a query parameter for retrieveCourse
runServerPartWithQuery :: ServerPart Response -> String -> IO Response
runServerPartWithQuery sp courseName = 
    runServerPartWith sp (mockRequestWithQuery courseName)

-- | Helper function to run ServerPart Response with a query parameter for courseInfo
runServerPartWithCourseInfoQuery :: ServerPart Response -> String -> IO Response
runServerPartWithCourseInfoQuery sp dept = 
    runServerPartWith sp (mockRequestWithCourseInfoQuery dept)

-- | Helper function to run ServerPart Response with a query parameter for retrieveProgram
runServerPartWithProgramQuery :: ServerPart Response -> String -> IO Response
runServerPartWithProgramQuery sp programCode = 
    runServerPartWith sp (mockRequestWithProgramQuery programCode)

-- | Helper function to run ServerPart Response with a body payload for findAndSavePrereqsResponse
runServerPartWithGraphGenerate :: ServerPart Response -> BSL.ByteString -> IO Response
runServerPartWithGraphGenerate sp payload = 
    runServerPartWith sp (mockRequestWithGraphGenerate payload)

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
    deleteWhere ([] :: [Filter Post])
    deleteWhere ([] :: [Filter PostCategory])
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
