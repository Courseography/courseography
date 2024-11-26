{-|
Description: Helper functions for various controller modules tests.

Module that contains helper functions used in testing controller module functions.

-}

module TestHelpers
    (mockRequest, runServerPart, clearDatabase) where

import Control.Concurrent.MVar (newMVar, newEmptyMVar)
import qualified Data.Map as Map
import Database.Persist.Sqlite (deleteWhere, Filter, SqlPersistM)
import Database.Tables
import Happstack.Server (HttpVersion(..), Request(..), simpleHTTP'', Response, ServerPart, Method(GET))

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
