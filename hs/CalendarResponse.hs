module CalendarResponse where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Time
import Happstack.Server
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Config (firstMondayFall, firstMondayWinter, filler)
import Database.CourseQueries (returnTutorialTimes, returnLectureTimes)
import qualified Data.Text as T
import Database.Tables as Tables

-- | Returns a CSV file of events as requested by the user.
calendarResponse :: String -> ServerPart Response
calendarResponse courses =
    liftIO $ getCalendar courses

-- Get together all the pieces of the program
getCalendar :: String -> IO Response
getCalendar courses = fmap genRes $ matchData (getNames courses) (startTimes $ allInfo courses) (endTimes $ allInfo courses) (startDates $ allInfo courses)

-- Generate a response
genRes :: [String] -> Response
genRes events =  toResponse $ take strLen fullStr
    where
    fullStr = unlines $ header ++ events
    strLen = (length fullStr) - 1
    header = ["Subject,Start Date,Start Time,End Date,End Time,All Day Event,Description,Location,Private"]

-- COOKIE INFORMATION

-- Obtain a list with all the information about the courses obtained from the cookies
getCoursesInfo :: String -> [[String]]
getCoursesInfo courses = map (splitOn "-") byCourse
    where
    byCourse = splitOn "_" courses

-- Obtain the name and session for all subjects
getNames :: String -> [(String, String)]
getNames courses = [(code, session) | [code, _ , session] <- getCoursesInfo courses] 

-- DATABASE INFORMATION

-- Obtain the information for all courses from the database
allInfo :: String -> [(IO [Time], (String, String))]
allInfo courses = [(pullDatabase code sect session, (code, session))| [code, sect, session] <- getCoursesInfo courses]

-- Pull out the information for each course from the database
pullDatabase :: String -> String -> String -> IO [Time]
pullDatabase code sect session =
    if (take 1 sect) == "L"
    then (returnLectureTimes (T.pack code) (T.pack sect) (T.pack session))
    else (returnTutorialTimes (T.pack code) (T.pack sect) (T.pack session))

-- ORDERING DATA

-- Order the data for each course
orderData :: [Time] -> [[[Double]]]
orderData courseFields = assignDay $ map timeField courseFields

assignDay :: [[Double]] -> [[[Double]]]
assignDay lst = [monday, tuesday, wednesday, thursday, friday]
    where
    monday = [field | field <- lst, field !! 0 == 0.0]
    tuesday = [field | field <- lst, field !! 0 == 1.0]
    wednesday = [field | field <- lst, field !! 0 == 2.0]
    thursday = [field | field <- lst, field !! 0 == 3.0]
    friday = [field | field <- lst, field !! 0 == 4.0]

-- START TIME

startTimes :: [(IO [Time], (String, String))] -> IO [[[String]]]
startTimes coursesTimes = sequence $ map startTime coursesTimes 

-- Obtain the starting time for each course
startTime :: (IO [Time], (String, String)) -> IO [[String]]
startTime courseTime =  fmap getDataStartTime $ fst courseTime

-- Obtain the data properly ordered
getDataStartTime :: [Time] -> [[String]]
getDataStartTime courseFields = getStartTime $ orderData courseFields

-- Obtaing a list of the startTimes for each day
getStartTime :: [[[Double]]] -> [[String]]
getStartTime  weekFields = [checkTimeStart courseField |courseField <- weekFields, not $ null courseField]

-- Get the start time
checkTimeStart :: [[Double]] -> [String]
checkTimeStart day = map getStr ([head sortedList] ++ getStartConsecutives sortedList)
    where
    sortedList = sort $ map (!! 1) day

getStartConsecutives :: [Double] -> [Double]
getStartConsecutives lst = filter (/= filler) ([if lst !! i == (lst !! (i + 1)) - 0.5 then filler else (lst !! (i + 1))|i <- [0 .. l]])
    where
    l = (length lst) - 2

-- END TIME

endTimes :: [(IO [Time], (String, String))] -> IO [[[String]]]
endTimes coursesTimes = sequence $ map endTime coursesTimes 

-- Obtain the ending time for each course
endTime :: (IO [Time], (String, String)) -> IO [[String]]
endTime courseTime =  fmap getDataEndTime $ fst courseTime

-- Obtain the data properly ordered
getDataEndTime :: [Time] -> [[String]]
getDataEndTime courseFields = getEndTime $ orderData courseFields

-- Obtaing a list of the startTimes for each day
getEndTime :: [[[Double]]] -> [[String]]
getEndTime  weekFields = [checkTimeEnd courseField |courseField <- weekFields, not $ null courseField]

-- Get the end time
checkTimeEnd :: [[Double]] -> [String]
checkTimeEnd day = map getStr (map (+ 0.5) (getEndConsecutives sortedList ++ [last sortedList]))
    where
    sortedList = sort $ map (!! 1) day

getEndConsecutives :: [Double] -> [Double]
getEndConsecutives lst = filter (/= filler) ([if lst !! i == (lst !! (i + 1)) - 0.5 then filler else lst !! i|i <- [0 .. l]])
    where
    l = (length lst) - 2

-- TIMES

-- Create the string time
getStr :: Double -> String
getStr fullTime = if minutes == 0 then getStrTime (hour1, ":00:00") else getStrTime (hour1, (":" ++ ratio minutes ++ ":00"))
    where
    hours = splitOn "." (show fullTime)
    hour1 = read (hours !! 0) :: Int 
    minutes = read (hours !! 1) :: Int 

-- Determine whether the time is AM or PM
getStrTime :: (Int, String) -> String
getStrTime (hour1, ending) = if hour1 >= 12 then (show $ afternoon hour1) ++ ending ++ " PM" else (show hour1) ++ ending ++ " AM"
    where
    afternoon hour1 = if hour1 == 12 then hour1 else hour1 - 12 

-- Get the time out of a decimal part of my time
ratio :: Int -> String
ratio decimal = if minutes >= 10 then show minutes else "0" ++ (show minutes)
    where
    minutes = decimal * 6

-- START/END DATE

startDates :: [(IO [Time], (String, String))] -> IO [([[String]], [[String]])]
startDates courseFields = sequence $ map (sequenceDates)  ([if (snd $ snd courseField) == "Y" then (halfFall courseField, halfWinter courseField) else (full courseField, return [[show filler]])| courseField <- courseFields])
    where
    halfFall courseField = startDate (fst courseField, (fst $ snd courseField, "F"))
    halfWinter courseField = startDate (fst courseField, (fst $ snd courseField, "S"))
    full courseField = startDate courseField

-- Similar to function sequence, but for tuples 
sequenceDates :: (IO [[String]], IO [[String]]) -> IO ([[String]], [[String]])
sequenceDates date = do
    fall <- fst date
    winter <- snd date
    return (fall, winter)

-- Obtain the starting time for each course
startDate :: (IO [Time], (String, String)) -> IO [[String]]
startDate courseField = fmap (getStartDate $ snd courseField) (fmap orderData (fst courseField))

-- Generate the string starting date
getStartDate :: (String, String) -> [[[Double]]] -> [[String]]
getStartDate codeSession courseFields = [getDate (snd codeSession) courseField |courseField <- courseFields, not $ null courseField]

-- Get a list of dates for each day
getDate :: String -> [[Double]] -> [String]
getDate session week = map format $ generateDate (getDay $ (concat week) !! 0) session

-- Give the appropriate day for the course given based on its position
getDay :: Double -> String 
getDay 0.0 = "M"
getDay 1.0 = "T"
getDay 2.0 = "W"
getDay 3.0 = "R"
getDay 4.0 = "F"
getDay _ = "That is not a valid representation of a day"

-- Format the date in the following way: month/day/year
format :: Day -> String
format date = formatTime defaultTimeLocale "%D" date

-- Takes data from event days to generate all the dates given the specific days
generateDate :: String -> String -> [Day]
generateDate courseDay "F" = generateDatesFall courseDay
generateDate courseDay "S" = generateDatesWinter courseDay
generateDate _ _  = []

-- Generate all the dates given the specific days (T.pack "F") "\"T\""
-- First day of classes will be on September 14.
-- Last day of classes will be on December 8
generateDatesFall :: String -> [Day]
generateDatesFall "M" = take 13 [addDays i firstMondayFall | i <- [0,7..]]
generateDatesFall "T" = take 13 [addDays i firstTuesday | i <- [0,7..]]
    where 
    firstTuesday = addDays 1 firstMondayFall
generateDatesFall "W" = take 12 [addDays i firstWednesday | i <- [0,7..]]
    where 
    firstWednesday = addDays 2 firstMondayFall
generateDatesFall "R" = take 12 [addDays i firstThursday | i <- [0,7..]]
    where 
    firstThursday = addDays 3 firstMondayFall
generateDatesFall "F" = take 12 [addDays i firstFriday | i <- [0,7..]]
    where 
    firstFriday = addDays 4 firstMondayFall
generateDatesFall _ = []

-- Generate all the dates given the specific days
-- First day of classes will be on January 11.
-- Last day of classes will be on April 8
generateDatesWinter :: String -> [Day]
generateDatesWinter "M" = take 13 [addDays i firstMondayWinter | i <- [0,7..]]
generateDatesWinter "T" = take 13 [addDays i firstTuesday | i <- [0,7..]]
    where 
    firstTuesday = addDays 1 firstMondayWinter 
generateDatesWinter "W" = take 13 [addDays i firstWednesday | i <- [0,7..]]
    where 
    firstWednesday = addDays 2 firstMondayWinter 
generateDatesWinter "R" = take 13 [addDays i firstThursday | i <- [0,7..]]
    where 
    firstThursday = addDays 3 firstMondayWinter 
generateDatesWinter "F" = take 13 [addDays i firstFriday | i <- [0,7..]]
    where 
    firstFriday = addDays 4 firstMondayWinter
generateDatesWinter _ = []

-- COMBINE INFORMATION

matchData :: [(String, String)] -> IO [[[String]]] -> IO [[[String]]] -> IO [([[String]], [[String]])] -> IO [String]
matchData namesSession allStart allEnd allDates = matchData1 namesSession (sequenceMatch ((allStart, allEnd), allDates))

matchData1 :: [(String, String)] -> IO (([[[String]]], [[[String]]]), [([[String]], [[String]])]) -> IO [String]
matchData1 namesSession allStartEndDates = fmap (matchInfo namesSession) allStartEndDates

matchInfo :: [(String, String)] -> (([[[String]]], [[[String]]]), [([[String]], [[String]])]) -> [String]
matchInfo namesSession allStartEndDates = matchInfo1 namesSession (fst $ fst allStartEndDates) (snd $ fst allStartEndDates) (snd allStartEndDates)

matchInfo1 :: [(String, String)] -> [[[String]]] -> [[[String]]] -> [([[String]], [[String]])] -> [String]
matchInfo1 namesSession start end dates = concat $ concat $ concat [eventsByCourse (namesSession !! i) (start !! i) (end !! i) (dates !! i)|  i <- [0 .. x]]
    where
    x = (length start) - 1

-- Events for each time
eventsByCourse :: (String, String) -> [[String]] -> [[String]] -> ([[String]], [[String]]) -> [[[String]]]
eventsByCourse namesSession starts ends dates = if snd namesSession == "Y" then year else half
    where
    year = halfFall ++ halfWinter
    halfFall = [eventsByCourse1 (fst namesSession) (starts !! i) (ends !! i) ((fst dates) !! i) | i <- [0 .. l]]
    halfWinter = [eventsByCourse1 (fst namesSession) (starts !! i) (ends !! i) ((snd dates) !! i) | i <- [0 .. l]]
    half = [eventsByCourse1 (fst namesSession) (starts !! i) (ends !! i) ((fst dates) !! i) | i <- [0 .. l]]
    l = (length starts) - 1

eventsByCourse1 :: String -> [String] -> [String] -> [String] -> [[String]]
eventsByCourse1 courseName start end date = [eventsByCourse2 courseName (start !! i) (end !! i) date | i <- [0 .. l]]
    where
    l = (length start) - 1

-- Generate the string that represents the event for each course
eventsByCourse2 :: String -> String -> String -> [String] -> [String]
eventsByCourse2 courseName start end date =  [courseName ++ "," ++ byDate ++ "," ++ start ++ "," ++ byDate ++ "," ++ end ++ ",False," ++ courseName ++ ",tba,True"| byDate <- date] 

-- Similar to fucntion sequence, but for tuples
sequenceMatch :: ((IO [[[String]]], IO [[[String]]]), IO [([[String]], [[String]])]) -> IO (([[[String]]], [[[String]]]), [([[String]], [[String]])])
sequenceMatch allStartEndDates = do
    start <- fst $ fst allStartEndDates
    end <- snd $ fst allStartEndDates
    dates <- snd allStartEndDates
    return ((start, end), dates)