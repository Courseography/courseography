{-# LANGUAGE OverloadedStrings #-}


module WebParsing.UtsgJsonParser
     (insertAllCourses) where

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

import Data.ByteString.Lazy hiding (take, foldl, map, elem)
import Data.Traversable
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

import Data.Map (Map)

 -- | URL to UofT courses (stored as JSON string)
jsonURL :: String
jsonURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?code=ABS210"

 -- | Decode JSON string into hash map object
getJSON :: IO (Maybe DB)
getJSON = do
  resp <- simpleHttp jsonURL
  return $ (decode resp :: Maybe DB)

instance FromJSON Courses where
  parseJSON = withObject "Courses" $ \o -> do
    code <- o .: "code"
    title  <- o .: "courseTitle"
    description  <- o .: "courseDescription"
    meetingsObj <- (o .: "meetings" :: Parser (HM.HashMap String Value))
    let manualTutorialEnrolment = elem "TUT" $ map (take 3) $ M.keys meetingsObj
        manualPracticalEnrolment = elem "PRA" $ map (take 3) $ M.keys meetingsObj
    prereqString <- o .: "prerequisite"
    let prereqs = parsePrerequisites $ Just prereqString
    exclusions <- o .: "exclusion"
    breadth <- o .: "breadthCategories"
    distribution <- o .: "distributionCategories"
    coreqs <- o .: "corequisite"
    let videoUrls = []
    return $ Courses code
                     (Just title)
                     (Just description)
                     (Just manualTutorialEnrolment)
                     (Just manualPracticalEnrolment)
                     prereqs
                     exclusions
                     (Just breadth)
                     (Just distribution)
                     (Just prereqString)
                     (Just coreqs)
                     videoUrls


newtype DB = DB (Map String Courses)
  deriving Show

instance FromJSON DB where
  parseJSON val = DB <$> parseJSON val

insertAllCourses = do
    coursesLst <- getJSON
    return $ case coursesLst of
                (Just (DB courses)) -> courses
                otherwise -> (() Map String Courses)




