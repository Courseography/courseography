module CalendarResponse where

import Data.List.Split (splitOn)
import Data.List
import Data.Time
import Happstack.Server
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Config (firstMondayFall, firstMondayWinter)
import Database.CourseQueries (returnCourse, returnTutorialTimes, returnLectureTimes)
import qualified Data.Text as T
import Database.Tables as Tables
import JsonResponse
import Data.Aeson (encode, decode)
import Database.Persist

-- Deal with 30.0, 8.0 and "invalid"
-- | Returns a CSV file of events as requested by the user.
calendarResponse :: String -> ServerPart Response
calendarResponse courses =
    liftIO $ getCalendar courses

-- Get together all the pieces of the program
getCalendar :: String -> IO Response
getCalendar courses = fmap (genRes) (matchData (getNames courses) (startTimes $ allInfo courses) (endTimes $ allInfo courses) (startDates $ allInfo courses))

-- Generate a response
genRes :: [String] -> Response
genRes events =  toResponse $ take all str
    where
    str = unlines events
    all = (length str) - 1 

-- Obtain the name and session for all subjects
getNames :: String -> [(String, String)]
getNames courses = [(code, session) | [code, section, session] <- getCoursesInfo courses] 

-- Obtain the information for all courses from the database
allInfo :: String -> [(IO [Time], (String, String))]
allInfo courses = [(pullDatabase code section session, (code, session))| [code, section, session] <- getCoursesInfo courses]

-- Obtain a list with all the information about the courses obtained from the cookies
getCoursesInfo :: String -> [[String]]
getCoursesInfo courses = map (splitOn "-") byCourse
    where
    byCourse = splitOn "_" courses

-- Pull out the information for each course from the database
pullDatabase :: String -> String -> String -> IO [Time]
pullDatabase code section session =
    if (take 1 section) == "L"
    then (returnLectureTimes (T.pack code) (T.pack section) (T.pack session))
    else (returnTutorialTimes (T.pack code) (T.pack section) (T.pack session))

-- ORDERING DATA

-- Order the data for each course
orderData :: (String, String) -> [Time] -> [[[Double]]]
orderData codeSession courseFields = orderDataCourse codeSession (map timeField courseFields)

orderDataCourse :: (String, String) -> [[Double]] -> [[[Double]]]
orderDataCourse codeSession courseFields = (filtering $ (groupDays courseFields))

-- Group them by day of the week and figure out consecutive blocks within that day
groupDays :: [[Double]] -> [[[Double]]]
groupDays courseFields = joinList ([assignDay (courseField) (courseField !! 0)| courseField <- courseFields])

-- Getting rid of unnecesary information (non- TimeFields)
filtering :: [[[Double]]] -> [[[Double]]]
filtering days = [filter p fields |fields <- days] 
    where p x = length x == 2 

-- Assign a position, which in turn represents a day
assignDay :: [Double] -> Double -> [[[Double]]]
assignDay courseField day
    | day <= 0.0 = [[courseField], [[8.0]], [[8.0]], [[8.0]], [[8.0]]]
    | day == 1.0 = [[[8.0]], [courseField], [[8.0]], [[8.0]], [[8.0]]]
    | day == 2.0 = [[[8.0]], [[8.0]], [courseField], [[8.0]], [[8.0]]]
    | day == 3.0 = [[[8.0]], [[8.0]], [[8.0]], [courseField], [[8.0]]]
    | otherwise = [[[8.0]], [[8.0]], [[8.0]], [[8.0]], [courseField]]

-- Create a final list that orderly contains the timeFields by day of the week
joinList :: [[[[Double]]]] -> [[[Double]]]
joinList days = [monday, tuesday, wednesday, thursday, friday] 
    where
    monday = concat $ map (!! 0) days 
    tuesday = concat $ map (!! 1) days
    wednesday = concat $ map (!! 2) days
    thursday = concat $ map (!! 3) days
    friday = concat $ map (!! 4) days

-- START TIME / END TIME

startTimes :: [(IO [Time], (String, String))] -> IO [[[String]]]
startTimes coursesTimes = sequence $ map startTime coursesTimes 

-- Obtain the starting time for each course
startTime :: (IO [Time], (String, String)) -> IO [[String]]
startTime courseTime =  fmap (getDataStartTime $ snd courseTime) (fst courseTime)

-- Obtain the data properly ordered
getDataStartTime :: (String, String) -> [Time] -> [[String]]
getDataStartTime codeSession courseFields = getStartTime (orderData codeSession courseFields) codeSession

-- Obtaing a list of the startTimes for each day
getStartTime :: [[[Double]]] -> (String, String) -> [[String]]
getStartTime  weekFields codeSession = [checkTimeStart codeSession courseField |courseField <- weekFields, not $ null courseField]

-- Get the start time (Smallest of the TimeFields)
checkTimeStart :: (String, String) -> [[Double]] -> [String]
checkTimeStart codeSession day = map getStr ([head sortedList] ++ getEndConsecutives sortedList)
    where
    sortedList = quicksort $ map (!! 1) day

getStartConsecutives :: [Double] -> [Double]
getStartConsecutives lst = filter (/= 30.0) ([if lst !! i == (lst !! (i - 1)) + 0.5 then 30.0 else lst !! i|i <- [l .. 1]])
    where
    l = (length lst) - 1

endTimes :: [(IO [Time], (String, String))] -> IO [[[String]]]
endTimes coursesTimes = sequence $ map endTime coursesTimes 

-- Obtain the ending time for each course
endTime :: (IO [Time], (String, String)) -> IO [[String]]
endTime courseTime =  fmap (getDataEndTime $ snd courseTime) (fst courseTime)

-- Obtain the data properly ordered
getDataEndTime :: (String, String) -> [Time] -> [[String]]
getDataEndTime codeSession courseFields = getEndTime (orderData codeSession courseFields) codeSession

-- Obtaing a list of the startTimes for each day
getEndTime :: [[[Double]]] -> (String, String) -> [[String]]
getEndTime  weekFields codeSession = [checkTimeEnd codeSession courseField |courseField <- weekFields, not $ null courseField]

-- Get the start time (Largest of the TimeFields)
checkTimeEnd :: (String, String) -> [[Double]] -> [String]
checkTimeEnd codeSession day = map getStr (map (+ 0.5) ([last sortedList] ++ getEndConsecutives sortedList))
    where
    sortedList = quicksort $ map (!! 1) day

getEndConsecutives :: [Double] -> [Double]
getEndConsecutives lst = filter (/= 30.0) ([if lst !! i == (lst !! (i + 1)) - 0.5 then 30.0 else lst !! i|i <- [0 .. l]])
    where
    l = (length lst) - 2

-- A nlogn logarithm for sorting
quicksort :: [Double] -> [Double]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y |y <- xs , y >= x]

-- Create the string time
getStr :: Double -> String
getStr time = if (length hours) == 1 then getStrTime (time , hours, ":00:00") else getStrTime (time , hours, (":" ++ (ratio (hours !! 1)) ++ ":00"))
    where
    hours = splitOn "." (show time)

-- Determine whether the time is AM or PM
getStrTime :: (Double, [String], String) -> String
getStrTime (time, hours, ending) = if time >= 12.0 then (hours !! 0) ++ ending ++ " PM" else (hours !! 0) ++ ending ++ " AM"

-- Get the time out of a decimal part of my time
ratio :: String -> String
ratio decimal = if minutes >= 10 then show minutes else "0" ++ (show minutes)
    where
    decimalDouble = read decimal :: Double
    minutes = floor $ decimalDouble * 6

-- DEALING WITH DATES. BE CAREFUL WITH ONE YEAR LECTURES AND TUTORIALS

startDates :: [(IO [Time], (String, String))] -> IO [([[String]], [[String]])]
startDates courseFields = sequence $ map (sequenceDates)  ([if (snd $ snd courseField) == "Y" then (halfFall courseField, halfWinter courseField) else (full courseField, return [["invalid"]])| courseField <- courseFields])
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
startDate courseField = fmap (getStartDate $ snd courseField) (fmap (orderData $ snd courseField) (fst courseField)) -- [[[Double]]]

-- Generate the string starting date
getStartDate :: (String, String) -> [[[Double]]] -> [[String]]
getStartDate codeSession courseFields = [getDate (snd codeSession) courseField |courseField <- courseFields, not $ null courseField]

-- Get a list of dates for each day
getDate :: String -> [[Double]] -> [String]
getDate session week = map format (generateDate (getDay $ (concat week) !! 0) session)

-- Give the appropriate day for the course given based on its position
getDay :: Double -> String 
getDay 0.0 = "M"
getDay 1.0 = "T"
getDay 2.0 = "W"
getDay 3.0 = "R"
getDay 4.0 = "F"

-- Format the date in the following way: month/day/year
format :: Day -> String
format date = formatTime defaultTimeLocale "%D" date

-- Takes data from event days to generate all the dates given the specific days
generateDate :: String -> String -> [Day]
generateDate courseDay "F" = generateDatesFall courseDay
generateDate courseDay "S" = generateDatesWinter courseDay

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
    half = [eventsByCourse1 (fst namesSession) (starts !! i) (ends !! i) ((fst dates) !! i) | i <- [0 .. l]] -- a list of dates for each day
    l = (length starts) - 1

eventsByCourse1 :: String -> [String] -> [String] -> [String] -> [[String]]
eventsByCourse1 name start end date = [eventsByCourse2 name (start !! i) (end !! i) date | i <- [0 .. l]]
    where
    l = (length start) - 1

-- Generate the string that represents the event for each course
eventsByCourse2 :: String -> String -> String -> [String] -> [String]
eventsByCourse2 name start end date =  [name ++ "," ++ byDate ++ "," ++ start ++ "," ++ byDate ++ "," ++ end ++ ",False," ++ name ++ ",tba,True"| byDate <- date] 

-- Similar to fucntion sequence, but for tuples
sequenceMatch :: ((IO [[[String]]], IO [[[String]]]), IO [([[String]], [[String]])]) -> IO (([[[String]]], [[[String]]]), [([[String]], [[String]])])
sequenceMatch allStartEndDates = do
    start <- fst $ fst allStartEndDates
    end <- snd $ fst allStartEndDates
    dates <- snd allStartEndDates
    return ((start, end), dates)

{-Test
ghci> let check = checkTimeStart ("my", "course") [[0.0,10.5],[0.0,11.5],[0.0,11
.0],[0.0,12.0],[0.0,12.5],[0.0,13.0]]
ghci> let check1 = checkTimeStart ("my", "course") [[0.0,10.5],[0.0,13.5],[0.0,1
1.0],[0.0,12.0],[0.0,12.5],[0.0,13.0]]
ghci> check
["10:30:00 AM"]
ghci> check1
["10:30:00 AM","11:00:00 AM"]-}