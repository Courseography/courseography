{-# LANGUAGE OverloadedStrings #-}


module WebParsing.UtsgJsonParser
     (getAllCourses) where

import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
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
import Control.Applicative ((<$>), (<*>))

import Data.ByteString.Lazy
import Data.Traversable
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

import Data.Map (Map)

 -- | URL to UofT courses (stored as JSON string)
jsonURL :: String
jsonURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?code=CSC108"

 -- | Decode JSON string into hash map object
getJSON :: IO (Maybe DB)
getJSON = do
  resp <- simpleHttp jsonURL
  return $ (decode resp :: Maybe DB)

instance FromJSON Course2 where
 parseJSON (Object v) =
    Course2 <$> v .: "code"
            <*> v .: "courseTitle"
            <*> v .: "courseDescription"
 parseJSON _ = mzero

newtype DB = DB (Map String Course2)
  deriving Show

instance FromJSON DB where
  parseJSON val = DB <$> parseJSON val

getAllCourses = undefined

