{-# LANGUAGE OverloadedStrings #-}

{-
module WebParsing.UtsgJsonParser
     (getJSON) where
-}

import Data.Aeson
import Data.List
import Database.Tables
import WebParsing.PrerequisiteParsing
import Data.Maybe
import Control.Monad
import Database.Persist.Sqlite (runSqlite, insertMany_, insert_)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import Config (databasePath)
import Network.HTTP.Conduit (simpleHttp)

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
getTimeSlots :: String -> String -> String -> [Time]
getTimeSlots day start end = do
    let dayDbl = getDayVal day
        startDbl = getHourVal start
        endDbl = getHourVal end
    [Time [dayDbl, timeDbl] | timeDbl <- [startDbl, (startDbl + 0.5) .. (endDbl - 0.5)]]

getNestedTimeSlots :: [Maybe T.Text] -> [Maybe T.Text] -> [Maybe T.Text] -> [[Time]]
getNestedTimeSlots [Nothing] [Nothing] [Nothing] = [[]]
getNestedTimeSlots days starts ends = zipWith3 (\(Just day)
                                                 (Just start)
                                                 (Just end) -> getTimeSlots (T.unpack day)
                                                                            (T.unpack start)
                                                                            (T.unpack end))
                                               days
                                               starts
                                               ends

 -- | URL to UofT courses (stored as JSON string)
jsonURL :: String
jsonURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?org=&code=PRT100Y"

 -- | Decode JSON string to JSON object
getJSON :: IO (Maybe Object)
getJSON = do
    resp <- simpleHttp jsonURL
    return $ decode resp

-- | Extract course metadata (values) from JSON object
-- | getMetadata :: IO (Maybe (M.HashMap k v)) -> IO [v]
getMetadata = fmap (\(Just v) -> M.elems v)

getValues = fmap (\(Just v) -> M.elems v)

-- | Extract course metadata (keys) from JSON object
-- | getKeys :: [Maybe Object] -> [T.Text]
getKeys = fmap (\(Just k) -> M.keys k)

 -- | Lookup field (by name) within metadata and then "expose" results.
--  | lookupField :: Functor f => Data.Text.Internal.Text -> f Value -> f (Maybe Data.Text.Internal.Text)
lookupField fieldName = do
        searchResults <- fmap (\val -> case val of
                                            (Object v) -> M.lookup fieldName v
                                            otherwise -> Nothing)
        -- [Maybe (Value a)] -> [Maybe a]
        return $ fmap (\val -> case val of
                            (Just (String v)) -> Just v
                            otherwise -> Nothing) searchResults

lookupObj fieldName = do
        searchResults <- fmap (\(Object v) -> M.lookup fieldName v)
        return $ fmap (\result -> case result of
                                    (Just (Object v)) -> Just v
                                    otherwise -> Nothing) searchResults

 -- | Retrieve all Course Table metadata
getAllCourses :: IO ()
getAllCourses = do
    print ("parsing JSON data from: " ++ jsonURL)
    allMetadata <- getMetadata getJSON

    let codes'                    = lookupField "code"                                       allMetadata
        codes                     = map (\(Just c) -> c)                                     codes'
    let courseTitles              = lookupField "courseTitle"                                allMetadata
        courseDescriptions        = lookupField "courseDescription"                          allMetadata
        prereqs                   = fmap        parsePrerequisites                           prereqStrings
        exclusions                = lookupField "exclusion"                                  allMetadata
        breadths                  = lookupField "breadthCategories"                          allMetadata
        distributions             = lookupField "distributionCategories"                     allMetadata
        prereqStrings             = lookupField "prerequisite"                               allMetadata
        coreqs                    = lookupField "corequisite"                                allMetadata
        sessions                  = lookupField "section"                                    allMetadata

    let allMeetings               = lookupObj "meetings"                                     allMetadata
    let sections                  = getKeys                                                  allMeetings
    let allMeetingMetadata        = getValues                                                allMeetings
    let allInstructors            = getValues $ fmap (fmap (lookupObj "instructors"))        allMeetings

    let allInstructorsFirstname'   = fmap (fmap (\val -> case val of
                                                            (Just v) -> lookupField "firstName" v
                                                            otherwise -> M.empty)) allInstructors
    let allInstructorsFirstname   = fmap (fmap M.elems) allInstructorsFirstname'

    let allInstructorsLastname'   = fmap (fmap (\val -> case val of
                                                            (Just v) -> lookupField "lastName" v
                                                            otherwise -> M.empty)) allInstructors
    let allInstructorsLastname   = fmap (fmap M.elems) allInstructorsLastname'


    let enrollmentCapacities      = fmap (lookupField "enrollmentCapacity")                  allMeetingMetadata
        waitlists'                = fmap (lookupField "waitlist")                            allMeetingMetadata
    let waitlists                 = map (map (\(Just v) -> (if v == "Y" then 0 else -1)))    waitlists'
        enrollments               = map (map (\(Just v) -> 0))                               waitlists'
        extras                    = enrollments

    let allSchedulesMetadata      = fmap (lookupObj "schedule")                              allMeetingMetadata
    let allMeetingMetadata        = fmap getValues                                           allSchedulesMetadata

    let meetingDays               = fmap (fmap (lookupField "meetingDay"))                   allMeetingMetadata
        meetingStartTimes         = fmap (fmap (lookupField "meetingStartTime"))             allMeetingMetadata
        meetingEndTimes           = fmap (fmap (lookupField "meetingEndTime"))               allMeetingMetadata

    let fullInstructorName        = zipWith (\firstNameSections
                                              lastNameSections -> zipWith (\firstNameSection
                                                                            lastNameSection -> if firstNameSection == []
                                                                                               then ""
                                                                                               else (T.unpack $ fromJust $ firstNameSection !! 0) ++ " " ++ (T.unpack $ fromJust $ lastNameSection !! 0))
                                                                          firstNameSections
                                                                          lastNameSections)
                                            allInstructorsFirstname
                                            allInstructorsLastname

    let times                     = zipWith3 (\courseDays
                                               courseStarts
                                               courseEnds -> zipWith3 (\dayLst
                                                                        startLst
                                                                        endLst -> getNestedTimeSlots dayLst startLst endLst)
                                                                      courseDays
                                                                      courseStarts
                                                                      courseEnds)
                                             meetingDays
                                             meetingStartTimes
                                             meetingEndTimes

    let timeStr                   = zipWith3 (\courseDays
                                               courseStarts
                                               courseEnds -> zipWith3 (\dayLst
                                                                        startLst
                                                                        endLst -> zipWith3 (\days start end -> if isJust days
                                                                                                               then (T.unpack $ fromJust days) ++ " " ++ (T.unpack $ fromJust start) ++ "-" ++ (T.unpack $ fromJust end)
                                                                                                               else "TBA")
                                                                                            dayLst
                                                                                            startLst
                                                                                            endLst)
                                                                      courseDays
                                                                      courseStarts
                                                                      courseEnds)
                                             meetingDays
                                             meetingStartTimes
                                             meetingEndTimes

    let manualTutorialEnrolments  = repeat      $ Just False
        manualPracticalEnrolments = repeat      $ Just False
    let videoUrls                 = repeat      ([] :: [T.Text])

    let lst7 = zipWith7 (\a b c d e f g -> a:b:c:d:e:f:g:[])
                         courseTitles
                         courseDescriptions
                         prereqs
                         exclusions
                         breadths
                         distributions
                         prereqStrings

    print ("inserting " ++ (show $ length lst7) ++ " courses into database")

    let codeSessions = zipWith (\code (Just session) -> code:session:[])
                               codes
                               sessions

    let sectionMetadata = zipWith6 (\section caps names enrolls waits extra -> zipWith6 (\sec (Just cap) name enroll wait ex -> (T.unpack sec):(T.unpack cap):name:(show enroll):(show wait):(show ex):[])
                                                                                        section
                                                                                        caps
                                                                                        names
                                                                                        enrolls
                                                                                        waits
                                                                                        extra)
                                    sections
                                    enrollmentCapacities
                                    fullInstructorName
                                    enrollments
                                    waitlists
                                    extras

    let lectureObjs = zipWith4 (\codeSession secMeta time tStrs -> zipWith3 (\sMeta ts tStr -> zipWith (\t ts -> if (take 3 $ sMeta !! 0) == "TUT"
                                                                                                                 then (Left $ Tutorial (codeSession !! 0)
                                                                                                                                       (Just $ T.pack $ sMeta !! 0)
                                                                                                                                       (codeSession !! 1)
                                                                                                                                       t)
                                                                                                                 else (Right $ Lecture (codeSession !! 0)
                                                                                                                                       (codeSession !! 1)
                                                                                                                                       (T.pack $ sMeta !! 0)
                                                                                                                                       t
                                                                                                                                       (read $ sMeta !! 1 :: Int)
                                                                                                                                       (T.pack $ sMeta !! 2)
                                                                                                                                       (read $ sMeta !! 3 :: Int)
                                                                                                                                       (read $ sMeta !! 4 :: Int)
                                                                                                                                       (read $ sMeta !! 5 :: Int)
                                                                                                                                       (T.pack ts)))
                                                                                                       ts
                                                                                                       tStr)
                                                                            secMeta
                                                                            time
                                                                            tStrs)
                               codeSessions
                               sectionMetadata
                               times
                               timeStr

    forM_ lectureObjs (\lecLists -> forM_ lecLists (\lecLst -> forM_ lecLst (\lecTut -> runSqlite databasePath $  case lecTut of
                                                                                                                        (Left tut) -> insert_ tut
                                                                                                                        (Right lec) -> insert_ lec)))

    runSqlite databasePath $ insertMany_ $ zipWith6 (\c lst corqs mTutEnrl mPratEnrl vUrl ->
                                                    Courses c (lst !! 0) (lst !! 1) mTutEnrl mPratEnrl (lst !! 2)
                                                            (lst !! 3) (lst !! 4) (lst !! 5) (lst !! 6) corqs vUrl)
                                                    codes
                                                    lst7
                                                    coreqs
                                                    manualTutorialEnrolments
                                                    manualPracticalEnrolments
                                                    videoUrls


    print ("All Courses have been successfully inserted")

