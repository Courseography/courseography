{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module WebParsing.UtsgJsonParser
     (getAllCourses,
      getOrgs) where

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
getNestedTimeSlots days starts ends = map nub $ zipWith3 (\(Just day)
                                                 (Just start)
                                                 (Just end) -> getTimeSlots (T.unpack day)
                                                                            (T.unpack start)
                                                                            (T.unpack end))
                                               days
                                               starts
                                               ends

 -- | URL to UofT courses (stored as JSON string)
jsonURL :: String
jsonURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?org="

 -- | Decode JSON string to JSON object
getJSON :: String -> IO (Maybe Object)
getJSON org = do
    resp <- simpleHttp (jsonURL ++ org)
    return $ decode resp

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


-- | Extract course metadata (values) from JSON object
-- | getMetadata :: IO (Maybe (M.HashMap k v)) -> IO [v]
getMetadata = fmap (\hm -> maybe [] M.elems hm)


getValues = fmap (\hm -> maybe [] M.elems hm)

-- | Extract course metadata (keys) from JSON object
-- | getKeys :: [Maybe Object] -> [T.Text]
getKeys = fmap (\hm -> maybe [] M.keys hm)

 -- | Lookup field (by name) within metadata and then "expose" results.
--  | lookupField :: Functor f => Data.Text.Internal.Text -> f Value -> f (Maybe Data.Text.Internal.Text)
lookupField fieldName = do
        searchResults <- fmap (\val -> case val of
                                            (Object v) -> M.lookup fieldName v
                                            otherwise -> Nothing)

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
    orgs <- getOrgs
    forM_ orgs (\org -> do
        print ("parsing JSON data from: " ++ jsonURL ++ org)
        allMetadata <- getMetadata (getJSON org)

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
        let waitlists                 = map (map (maybe (-1) (\v -> (if v == "Y" then 0 else -1))))  waitlists'
            enrollments               = map (map (\_ -> 0))                               waitlists'
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

        let times                   = zipWith3 (\courseDays
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

        let sectionMetadata = zipWith6 (\section caps names enrolls waits extra -> zipWith6 (\sec cap name enroll wait ex -> (T.unpack sec):(T.unpack (fromMaybe "0" cap)):name:(show enroll):(show wait):(show ex):[])
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

        let lectureObjs = zipWith4 (\codeSession secMeta timeList tStrs -> zipWith3 (\sMeta tms tStr -> if (take 3 $ sMeta !! 0) == "TUT" || (take 3 $ sMeta !! 0) == "PRA"
                                                                                                    then (Left $ Tutorial (codeSession !! 0)
                                                                                                                          (Just $ T.pack $ sMeta !! 0)
                                                                                                                          (codeSession !! 1)
                                                                                                                          (nub $ concat tms))
                                                                                                    else (Right $ Lecture (codeSession !! 0)
                                                                                                                          (codeSession !! 1)
                                                                                                                          (T.pack $ sMeta !! 0)
                                                                                                                          (nub $ concat tms)
                                                                                                                          (read $ sMeta !! 1 :: Int)
                                                                                                                          (T.pack $ sMeta !! 2)
                                                                                                                          (read $ sMeta !! 3 :: Int)
                                                                                                                          (read $ sMeta !! 4 :: Int)
                                                                                                                          (read $ sMeta !! 5 :: Int)
                                                                                                                          (T.pack $ intercalate ", " tStr)))
                                                                                secMeta
                                                                                timeList
                                                                                tStrs)
                                   codeSessions
                                   sectionMetadata
                                   times
                                   timeStr

        let manualTutorialEnrolments  =  map (\lecLists -> foldl (\tutExists lecTut -> case lecTut of
                                                                                              (Left tut) -> if (take 3 $ T.unpack $ fromJust $ tutorialSection tut) == "TUT"
                                                                                                            then (Just True)
                                                                                                            else tutExists
                                                                                              (Right lec) -> tutExists) (Just False) lecLists) lectureObjs

            manualPracticalEnrolments = map (\lecLists -> foldl (\tutExists lecTut -> case lecTut of
                                                                                              (Left tut) -> if (take 3 $ T.unpack $ fromJust $ tutorialSection tut) == "PRA"
                                                                                                            then (Just True)
                                                                                                            else tutExists
                                                                                              (Right lec) -> tutExists) (Just False) lecLists) lectureObjs

        forM_ lectureObjs (\lecLists -> forM_ lecLists (\lecTut -> runSqlite databasePath $  case lecTut of
                                                                                                  (Left tut) -> insert_ tut
                                                                                                  (Right lec) -> insert_ lec))

        -- NOTE: This is still done in ArtSciParser (timetable doesn't have info for courses that aren't offered)
        -- let courses = zipWith6 (\c lst corqs mTutEnrl mPratEnrl vUrl ->
        --                                                 Courses c (lst !! 0) (lst !! 1) mTutEnrl mPratEnrl (lst !! 2)
        --                                                         (lst !! 3) (lst !! 4) (lst !! 5) (lst !! 6) corqs vUrl)
        --                                                 codes
        --                                                 lst7
        --                                                 coreqs
        --                                                 manualTutorialEnrolments
        --                                                 manualPracticalEnrolments
        --                                                 videoUrls
        --     finalCourses = nubBy (\a b -> coursesCode a == coursesCode b) courses
        -- runSqlite databasePath $ mapM_ insertMany_ $ splitEvery 50 finalCourses
        )
    print "All Courses have been successfully inserted"
    where
        splitEvery _ [] = []
        splitEvery n list =
            let (first, rest) = splitAt n list
            in
                first : (splitEvery n rest)
