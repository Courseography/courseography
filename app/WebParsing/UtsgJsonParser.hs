{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, GeneralizedNewtypeDeriving
  #-}

module WebParsing.UtsgJsonParser
     (getAllCourses,
      getOrgs,
      insertAllCourses) where

import Data.Aeson ((.:?), (.!=), decode, FromJSON(parseJSON), Value(..), Object)
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers, rights)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Conduit (simpleHttp)
import Config (databasePath)
import Database.Tables (Courses(..), Lecture(..), Tutorial(..))
import Database.Persist.Sqlite (runSqlite, insert_, SqlPersistM)

-- | URLs for the Faculty of Arts and Science API
timetableURL :: T.Text
timetableURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?code="

orgURL :: String
orgURL = "https://timetable.iit.artsci.utoronto.ca/api/orgs"

-- | Parse all timetable data.
getAllCourses :: IO ()
getAllCourses = do
    orgs <- getOrgs
    runSqlite databasePath $ mapM_ insertAllCourses orgs

-- | Return a list of all the "orgs" in FAS. These are the values which can be
--   passed to the timetable API with the "org" key.
getOrgs :: IO [T.Text]
getOrgs = do
    resp <- simpleHttp orgURL
    let rawJSON :: Maybe (HM.HashMap T.Text Object) = decode resp
    return $ maybe [] (concatMap HM.keys . HM.elems) rawJSON

-- | Retrieve and store all timetable data for the given department.
insertAllCourses :: T.Text -> SqlPersistM ()
insertAllCourses org = do
    liftIO $ print $ T.append "parsing JSON data from: " org
    resp <- liftIO $ simpleHttp $ T.unpack (T.append timetableURL org)
    let coursesLst :: Maybe (HM.HashMap T.Text (Maybe DB)) = decode resp
    let courseData = maybe [] (map dbData . catMaybes . HM.elems) coursesLst
    -- courseData contains courses and sections;
    -- only sections are currently stored here.
    let (_, sections) = unzip courseData
    let (lectures, tutorials) = partitionEithers $ concat sections

    mapM_ insert_ lectures
    mapM_ insert_ tutorials


newtype DB = DB { dbData :: (Courses, [Either Lecture Tutorial]) }
  deriving Show

instance FromJSON DB where
    parseJSON (Object o) = do
      course <- parseJSON (Object o)
      session :: T.Text <- o .:? "section" .!= "F"
      meetingMap :: HM.HashMap T.Text Meeting <- o .:? "meetings" .!= HM.empty
      let meetings = map (setCode (coursesCode course) session . meeting) (HM.elems meetingMap)
          -- Fix manualTutorialEnrolment and manualPracticalEnrolment
          manTut = any (maybe False (T.isPrefixOf "TUT") . tutorialSection) $ rights meetings
          manPra = any (maybe False (T.isPrefixOf "PRA") . tutorialSection) $ rights meetings
      return $ DB (course { coursesManualTutorialEnrolment = Just manTut,
                            coursesManualPracticalEnrolment = Just manPra },
                   meetings)
        where
          setCode code session (Left lec) = Left (lec { lectureCode = code,
            lectureSession = session} )
          setCode code session (Right tut) = Right (tut { tutorialCode = code,
            tutorialSession = session} )
    parseJSON _ = fail "Invalid section"

newtype Meeting = Meeting { meeting :: (Either Lecture Tutorial) }
  deriving Show

instance FromJSON Meeting where
  parseJSON x =
    (parseJSON x >>= return . Meeting . Left) <|> (parseJSON x >>= return . Meeting . Right)
