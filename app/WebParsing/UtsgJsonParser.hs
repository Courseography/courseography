{-# LANGUAGE OverloadedStrings #-}


module WebParsing.UtsgJsonParser
     (getAllCourses) where

import Data.Aeson
import Data.List
import Database.Tables
import WebParsing.PrerequisiteParsing
import Data.Maybe
import Data.String
import Control.Monad
import Database.Persist.Sqlite (runSqlite, insertMany_, insert_)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import Config (databasePath)
import Network.HTTP.Conduit (simpleHttp)

 -- | URL to UofT courses (stored as JSON string)
jsonURL :: String
jsonURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?code=CSC108"

-- | Remove Object constructor from list contents
fromObj :: [Value] -> [Object]
fromObj [] = []
fromObj (Object x:xs) = x : fromObj xs

-- | Remove String constructor from string literal
fromStr :: Value -> T.Text
fromStr (String str) = str

 -- | Decode JSON string into hash map object
getJSON :: IO (Maybe Object)
getJSON = do
    resp <- simpleHttp jsonURL
    return $ decode resp

-- | Return a list of values (from a hash map)
getValues :: IO (Maybe (M.HashMap k Value)) -> IO [Object]
getValues jsonIO = do
  rawJson <- jsonIO
  return $ fromObj $ M.elems $ fromJust rawJson

-- look-up value within hash map
lookupVal hash val = fromStr $ fromJust $ M.lookup val hash

lookupVals valLst hash = map (lookupVal hash) valLst

insertCourses hashIO = do
    hashVals <- hashIO
    let courseMetadata =  map (lookupVals ["code",
                                           "courseTitle",
                                           "courseDescription",
                                           "exclusion",
                                           "breadthCategories",
                                           "distributionCategories",
                                           "prerequisite",
                                           "corequisite"
                                           ]) hashVals

    let prereqs = map (\course -> parsePrerequisites $ Just $ course !! 6) courseMetadata
{-
    runSqlite databasePath $ insertMany_ $ zipWith5 (\crseLst prereqs mTutEnrl mPratEnrl vUrl ->
                                                     Courses (crseLst !! 0)
                                                             (Just $ crseLst !! 1)
                                                             (Just $ crseLst !! 2)
                                                             mTutEnrl
                                                             mPratEnrl
                                                             prereqs
                                                             (Just $ crseLst !! 4)
                                                             (Just $ crseLst !! 5)
                                                             (Just $ crseLst !! 6)
                                                             (Just $ crseLst !! 7)
                                                             (Just $ crseLst !! 8)
                                                             vUrl)
                                                    courseMetadata
                                                    prereqs
                                                    repeat $ Just False
                                                    repeat $ Just False
                                                    repeat []
-}
    return courseMetadata


getAllCourses = undefined
