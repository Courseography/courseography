{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, GeneralizedNewtypeDeriving
  #-}

module WebParsing.UtsgJsonParser
     (getAllCourses,
      getOrgs,
      insertAllCourses) where

import Data.Aeson ((.:?), (.!=), decode, FromJSON(parseJSON), Value(..), Object)
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers, rights)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Text.Read (readMaybe)
import Network.HTTP.Conduit (simpleHttp)

import Config (databasePath)
import Database.Tables (Courses(..), Lecture(..), Tutorial(..), Time(..))
import Database.Persist.Sqlite (runSqlite, insert_)


-- | URLs for the Faculty of Arts and Science API
timetableURL :: String
timetableURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?code="

orgURL :: String
orgURL = "https://timetable.iit.artsci.utoronto.ca/api/orgs"


-- | Parse all timetable data.
getAllCourses :: IO ()
getAllCourses = do
    orgs <- getOrgs
    mapM_ (insertAllCourses . T.unpack) orgs

-- | Return a list of all the "orgs" in FAS. These are the values which can be
--   passed to the timetable API with the "org" key.
getOrgs :: IO [T.Text]
getOrgs = do
    resp <- simpleHttp orgURL
    let rawJSON :: Maybe Object = decode resp
    return $ maybe [] HM.keys rawJSON

-- | Retrieve and store all timetable data for the given department.
insertAllCourses :: String -> IO ()
insertAllCourses org = do
    print $ "parsing JSON data from: " ++ org
    resp <- simpleHttp (timetableURL ++ org)
    let coursesLst :: Maybe (HM.HashMap T.Text DB) = decode resp
    let courseData = maybe [] (map dbData . HM.elems) coursesLst
    -- courseData contains courses and sections;
    -- only sections are currently stored here.
    let (_, sections) = unzip courseData
    let (lectures, tutorials) = partitionEithers $ concat sections
    runSqlite databasePath (do
        mapM_ insert_ lectures
        mapM_ insert_ tutorials)


-- | Converts 24-hour time into a double
-- | Assumes times are rounded to the nearest hour
getHourVal :: String -> Double
getHourVal time = (read $ take 2 time :: Double) + (/) (read $ drop 3 time :: Double) 60

-- | Converts a weekday into a double
-- | Monday to Friday becomes 0.0 to 4.0
getDayVal :: String -> Double
getDayVal "MO" = 0.0
getDayVal "TU" = 0.0
getDayVal "WE" = 0.0
getDayVal "TH" = 0.0
getDayVal "FR" = 0.0
getDayVal _    = 4.0

-- | Takes a day and start/end times then generates a set of 30-minute timeslots
getTimeSlots :: Maybe String -> Maybe String -> Maybe String -> [Time]
getTimeSlots (Just day) (Just start) (Just end) = do
    let dayDbl = getDayVal day
        startDbl = getHourVal start
        endDbl = getHourVal end
    [Time [dayDbl, timeDbl] | timeDbl <- [startDbl, (startDbl + 0.5) .. (endDbl - 0.5)]]
getTimeSlots _ _ _ = []


newtype DB = DB { dbData :: (Courses, [Either Lecture Tutorial]) }
  deriving Show

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

newtype Meeting = Meeting { meeting :: (Either Lecture Tutorial) }
  deriving Show

instance FromJSON Meeting where
    parseJSON (Object o) = do
      teachingMethod :: T.Text <- o .:? "teachingMethod" .!= ""
      sectionNumber :: T.Text <- o .:? "sectionNumber" .!= ""
      timeMap :: Value <- o .:? "schedule" .!= Null
      timeslots <- case timeMap of
              Object obj -> mapM parseJSON (HM.elems obj)
              _ -> return []
      let allTimes = concatMap times timeslots
      let sectionId = T.concat [teachingMethod, sectionNumber]
      case teachingMethod of
        "LEC" -> do
          enrollmentCapacity <- o .:? "enrollmentCapacity" .!= "-1"
          actualEnrolment <- o .:? "actualEnrolment" .!= "0"
          actualWaitlist <- o .:? "actualWaitlist" .!= "0"
          instrMap2 :: Value <- o .:? "instructors" .!= Null
          let instrList =
                case instrMap2 of
                  Object obj -> HM.elems obj
                  _ -> []

          instrs <- mapM parseInstr instrList
          let cap = fromMaybe (-1) $ readMaybe enrollmentCapacity
          let enrol = fromMaybe 0 $ readMaybe actualEnrolment
          let wait = fromMaybe 0 $ readMaybe actualWaitlist
          let extra = 0
          let timeStr = ""
          let instructor = T.intercalate "; " $ filter (not . T.null) instrs
          return $ Meeting $ Left $ Lecture "" "" sectionId allTimes cap instructor enrol wait extra timeStr
        --"TUT"
        _ ->
          return $ Meeting $ Right $ Tutorial "" (Just sectionId) "" allTimes
      where
        parseInstr (Object io) = do
          firstName <- io .:? "firstName" .!= ""
          lastName <- io .:? "lastName" .!= ""
          return (T.concat [firstName, ". ", lastName])
        parseInstr _ = return ""
    parseJSON _ = error "Invalid meeting"


newtype TimeSlot = TimeSlot { times :: [Time] }

instance FromJSON TimeSlot where
  parseJSON (Object o) = do
    meetingDay <- o .:? "meetingDay"
    meetingStartTime <- o .:? "meetingStartTime"
    meetingEndTime <- o .:? "meetingEndTime"
    return $ TimeSlot $ getTimeSlots meetingDay meetingStartTime meetingEndTime
  parseJSON _ = error "Invalid TimeSlot"
