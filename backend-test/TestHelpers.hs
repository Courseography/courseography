{-|
Description: Helper functions for various controller modules tests.

Module that contains helper functions used in testing controller module functions.

-}

module TestHelpers
    (mockRequest, runServerPart, clearDatabase, runServerPartWithQuery) where

import Control.Concurrent.MVar (newMVar, newEmptyMVar)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import Database.Persist.Sqlite (deleteWhere, Filter, SqlPersistM)
import Database.Tables
import Happstack.Server (HttpVersion(..), Request(..), Input(..), Response, ServerPart, Method(GET), ContentType(..), simpleHTTP'', inputContentType, inputFilename, inputValue)

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
        , rqQuery           = "name=" ++ courseName
        , rqInputsQuery     = [("name", Input {
            inputValue = Right (BSL.pack courseName),
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