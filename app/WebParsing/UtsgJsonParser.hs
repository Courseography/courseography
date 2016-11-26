{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, GeneralizedNewtypeDeriving
  #-}

module WebParsing.UtsgJsonParser
     (getAllCourses,
      getOrgs,
      insertAllCourses) where

import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.List
import Database.Tables
import WebParsing.PrerequisiteParsing
import Data.Maybe
import Data.Either (partitionEithers, rights)
import Data.String
import Control.Monad
import Database.Persist.Sqlite (runSqlite, insertMany_, insert_)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import Text.Read (readMaybe)
import Config (databasePath)
import Network.HTTP.Conduit (simpleHttp)
import Control.Applicative ((<$>), (<*>))

import Data.Traversable
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

-- | URL to UofT courses (stored as JSON string)
jsonURL :: String
jsonURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?code="

-- | Converts 24-hour time into a double
-- | Assumes times are rounded to the nearest hour
getHourVal :: String -> Double
getHourVal time = (read $ take 2 time :: Double) + (/) (read $ drop 3 time :: Double) 60

-- | Converts a weekday into a double
-- | Monday to Friday becomes 0.0 to 4.0
getDayVal :: String -> Double
getDayVal day = case day of
                    "MO" -> 0.0
                    "TU" -> 1.0
                    "WE" -> 2.0
                    "TH" -> 3.0
                    "FR" -> 4.0

-- | Takes a day and start/end times then generates a set of 30-minute timeslots
getTimeSlots :: Maybe String -> Maybe String -> Maybe String -> [Time]
getTimeSlots (Just day) (Just start) (Just end) = do
    let dayDbl = getDayVal day
        startDbl = getHourVal start
        endDbl = getHourVal end
    [Time [dayDbl, timeDbl] | timeDbl <- [startDbl, (startDbl + 0.5) .. (endDbl - 0.5)]]
getTimeSlots _ _ _ = []


-- | Return a list of all the "orgs" in FAS. These are the values which can be
--   passed to the timetable API with the "org" key.
getOrgs :: IO [String]
getOrgs = do
    resp <- simpleHttp "https://timetable.iit.artsci.utoronto.ca/api/orgs"
    let rawJSON :: Maybe Object = decode resp
        orgJSON = maybe [] M.elems rawJSON
        orgs = case head orgJSON of
                  (Object v) -> map T.unpack $ M.keys v
                  _ -> []
    return orgs


-- | Parse all timetable data.
getAllCourses :: IO ()
getAllCourses = do
    orgs <- getOrgs
    forM_ orgs (\org -> do
        print ("parsing JSON data from: " ++ jsonURL ++ org)
        insertAllCourses org)

instance FromJSON Courses where
  parseJSON (Object o) = do
    code <- o .:? "code" .!= "CSC???"
    title  <- o .:? "courseTitle"
    description  <- o .:? "courseDescription"
    let manualTutorialEnrolment = False
    let manualPracticalEnrolment = False
    prereqString <- o .:? "prerequisite"
    let prereqs = parsePrerequisites prereqString
    exclusions <- o .:? "exclusion"
    coreqs <- o .:? "corequisite"
    let videoUrls = []
    return $ Courses code
                     title
                     description
                     (Just manualTutorialEnrolment)
                     (Just manualPracticalEnrolment)
                     prereqs
                     exclusions
                     Nothing -- breadth
                     Nothing -- distribution
                     Nothing -- (Just prereqString)
                     coreqs
                     videoUrls
  parseJSON _ = undefined

newtype DB = DB { dbData :: (Courses, [Either Lecture Tutorial]) }
  deriving Show

newtype Meeting = Meeting { meeting :: (Either Lecture Tutorial) }
  deriving Show

newtype TimeSlot = TimeSlot { times :: [Time] }

instance FromJSON TimeSlot where
  parseJSON (Object o) = do
    meetingDay <- o .:? "meetingDay"
    meetingStartTime <- o .:? "meetingStartTime"
    meetingEndTime <- o .:? "meetingEndTime"
    return $ TimeSlot $ getTimeSlots meetingDay meetingStartTime meetingEndTime


instance FromJSON DB where
    parseJSON (Object o) = do
      course <- parseJSON (Object o)
      session :: T.Text <- o .:? "section" .!= "CSC???"
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
    parseJSON _ = error "Invalid section"

instance FromJSON Meeting where
    parseJSON (Object o) = do
      teachingMethod :: T.Text <- o .:? "teachingMethod" .!= ""
      sectionNumber :: T.Text <- o .:? "sectionNumber" .!= ""
      timeMap :: Value <- o .:? "schedule" .!= Null
      timeslots <- case timeMap of
              Object o -> mapM parseJSON (HM.elems o)
              _ -> return []
      let allTimes = concatMap times timeslots
      let sectionId = T.concat [teachingMethod, sectionNumber]
      case teachingMethod of
        "LEC" -> do
          enrollmentCapacity <- o .:? "enrollmentCapacity" .!= "-1"
          actualEnrolment <- o .:? "actualEnrolment" .!= "0"
          actualWaitlist <- o .:? "actualWaitlist" .!= "0"
          instrMap2 :: Value <- o .:? "instructors" .!= Null
          --let instrMap2 = Null
          let instrList =
                case instrMap2 of
                  Object o -> HM.elems o
                  _ -> []

          --let instrMap = HM.empty
          instrs <- mapM parseInstr instrList
          let cap :: Int = fromMaybe (-1) $ readMaybe enrollmentCapacity
          let enrol :: Int = fromMaybe 0 $ readMaybe actualEnrolment
          let wait :: Int = fromMaybe 0 $ readMaybe actualWaitlist
          let extra = 0
          let timeStr = ""
          let instructor = T.intercalate "; " $ filter (not . T.null) instrs
          return $ Meeting $ Left $ Lecture "" "" sectionId allTimes cap instructor enrol wait extra timeStr
        --"TUT" -> do
        _ ->
          return $ Meeting $ Right $ Tutorial "" (Just sectionId) "" allTimes
      where
        parseInstr (Object io) = do
          firstName <- io .:? "firstName" .!= ""
          lastName <- io .:? "lastName" .!= ""
          return (T.concat [firstName, ". ", lastName])
        parseInstr _ = return ""
    parseJSON _ = error "Invalid meeting"

-- | Retrieve and store all timetable data for the given department.
insertAllCourses :: String -> IO ()
insertAllCourses org = do
    resp <- simpleHttp (jsonURL ++ org)
    let coursesLst :: Maybe (HM.HashMap T.Text DB) = decode resp
    let courseData = maybe [] (map dbData . HM.elems) coursesLst
    let (courses, sections) = unzip courseData
    let (lectures, tutorials) = partitionEithers $ concat sections
    runSqlite databasePath (do
        mapM_ insert_ lectures
        mapM_ insert_ tutorials)
