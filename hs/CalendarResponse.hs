module CalendarResponse where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Time (Day, addDays, formatTime)
import Happstack.Server (ServerPart, Response, toResponse)
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Config (firstMondayFall, firstMondayWinter)
import Database.CourseQueries (returnTutorialTimes, returnLectureTimes)
import qualified Data.Text as T
import Database.Tables as Tables

-- | Returns a CSV file of events as requested by the user.
calendarResponse :: String -> ServerPart Response
calendarResponse courses =
    liftIO $ getCalendar courses

-- | Gets together all the pieces of the program.
getCalendar :: String -> IO Response
getCalendar courses = do
    let courseInfo = getCoursesInfo courses
    databaseInfo <- sequence $ map pullDatabase courseInfo
    let events = concat $ map getEvent databaseInfo
    return $ toResponse $ getCSV $ events

-- | Generates a string representing a CSV file.
getCSV :: [String] -> String
getCSV events =  unlines $ header : events
    where
    header = "Subject,Start Date,Start Time,End Date,End Time,All Day Event,Description,Location,Private"

-- ** Cookie Information

-- | Obtains a list with all the information about the courses obtained from the cookies.
getCoursesInfo :: String -> [(String, String, String)]
getCoursesInfo courses = map courseInfo allCourses
    where
    courseInfo course = (course !! 0, course !! 1, course !! 2)
    allCourses = map (splitOn "-") byCourse
    byCourse = splitOn "_" courses

-- ** Database Information

-- | Pulls out the information for each course from the database.
pullDatabase :: (String, String, String) -> IO ([Time], String, String)
pullDatabase (code, sect, session) =
    if take 1 sect == "L"
    then getData $ returnLectureTimes (T.pack code) (T.pack sect) (T.pack session)
    else getData $ returnTutorialTimes (T.pack code) (T.pack sect) (T.pack session)
    where
    getData info = do
        courseInfo <- info 
        return (courseInfo, code, session)

-- ** Event Creation

-- | Generates an event for each course.
getEvent :: ([Time], String, String) -> [String]
getEvent (timeFields, code, session) = concat $ concat $ eventsByCourse code session start end dates
    where
    dataInOrder = orderData timeFields
    start = startTime dataInOrder
    end = endTime dataInOrder
    dates = startDates session dataInOrder

-- | Creates an event for each course
eventsByCourse :: String -> String -> [[String]] -> [[String]] -> [[[String]]] -> [[[String]]]
eventsByCourse code session start end dates = map (eventsByDay code session) (zip' start end dates)

-- | Join three lists together in tuples of three elements
zip' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip' _ _ [] = []
zip' _ [] _ = []
zip' [] _ _ = []
zip' (x:xs) (y2:ys) (z:zs) = (x,y2,z):zip' xs ys zs

-- | Creates an event for each day
eventsByDay :: String -> String -> ([String], [String], [[String]]) -> [[String]]
eventsByDay code session (start, end, dates) = if session == "Y" then year else half
    where
    year = halfFall ++ halfWinter
    halfFall = eventsByTime code start end (dates !! 0)
    halfWinter = eventsByTime code start end (dates !! 1)
    half = eventsByTime code start end (dates !! 0)

-- | Creates an event for each start/end time
eventsByTime :: String -> [String] -> [String] -> [String] -> [[String]]
eventsByTime code start end date = map (eventsByDate code date) (zip start end)

-- | Generates the string that represents the event for each course
eventsByDate :: String -> [String] -> (String, String) -> [String]
eventsByDate code dates (start, end) = map str dates
    where
    str date = code ++ "," ++ date ++ "," ++ start ++ "," ++ date ++ "," ++ end ++ ",False," ++ code ++ ",tba,True" 

-- ** Ordering data

-- | Orders the data for each course.
orderData :: [Time] -> [[[Double]]]
orderData courseFields = filter notNull (assignDay $ map timeField courseFields)
    where
    notNull field = not $ null field

-- | Organizes the data by the day of the week, assigning a position based on the time field day.
assignDay :: [[Double]] -> [[[Double]]]
assignDay lst = [monday, tuesday, wednesday, thursday, friday]
    where
    monday = let mon field = field !! 0 == 0.0 in filter mon lst
    tuesday = let tues field = field !! 0 == 1.0 in filter tues lst
    wednesday = let wed field = field !! 0 == 2.0 in filter wed lst
    thursday = let thur field = field !! 0 == 3.0  in filter thur lst
    friday = let fri field = field !! 0 == 4.0 in filter fri lst

-- ** Start time

-- | Obtains a list of start times for each course
startTime :: [[[Double]]] -> [[String]]
startTime weekFields = map checkTimeStart weekFields

-- | Gets all the start times for each day
checkTimeStart :: [[Double]] -> [String]
checkTimeStart day = map getStr ([head sortedList] ++ getStartConsecutives sortedList)
    where
    sortedList = sort $ map (!! 1) day

-- | Gets the start times that are not the very first start time 
getStartConsecutives :: [Double] -> [Double]
getStartConsecutives lst = filter (/= 30.0) ([if lst !! i == (lst !! (i + 1)) - 0.5 then 30.0 else (lst !! (i + 1))| i <- [0 .. l]])
    where
    l = (length lst) - 2

-- ** End time

-- | Obtains a list of end times for each course
endTime :: [[[Double]]] -> [[String]]
endTime weekFields = map checkTimeEnd weekFields

-- | Gets all the end times for each day
checkTimeEnd :: [[Double]] -> [String]
checkTimeEnd day = map getStr (map (+ 0.5) (getEndConsecutives sortedList ++ [last sortedList]))
    where
    sortedList = sort $ map (!! 1) day

-- | Gets the end times that are not the very first end time 
getEndConsecutives :: [Double] -> [Double]
getEndConsecutives lst = filter (/= 30.0) ([if lst !! i == (lst !! (i + 1)) - 0.5 then 30.0 else lst !! i| i <- [0 .. l]])
    where
    l = (length lst) - 2

-- ** Function that works for both start and end times

-- | Creates the string time
getStr :: Double -> String
getStr fullTime = if minutes == 0
                  then getStrTime (hour, ":00:00")
                  else getStrTime (hour, (":" ++ ratio minutes ++ ":00"))
    where
    hours = splitOn "." (show fullTime)
    hour = read (hours !! 0) :: Int 
    minutes = read (hours !! 1) :: Int 

-- | Determines whether the time is AM or PM
getStrTime :: (Int, String) -> String
getStrTime (hour, ending) = if hour >= 12
                            then (show $ afternoon hour) ++ ending ++ " PM"
                            else (show hour) ++ ending ++ " AM"
    where
    afternoon hour1 = if hour1 == 12 then hour1 else hour1 - 12 

-- | Gets the minutes out of a decimal part of my time
ratio :: Int -> String
ratio decimal = if minutes >= 10 then show minutes else "0" ++ (show minutes)
    where
    minutes = decimal * 6

-- ** Start/End date

-- | Obtains all the dates for each course depending on its session
startDates :: String -> [[[Double]]] -> [[[String]]]
startDates session dataInOrder = map (checkSession session) dataInOrder
    where
    checkSession session1 courseFields = if session1 == "Y" 
                                        then [halfFall courseFields, halfWinter courseFields]
                                        else [full session1 courseFields]
    halfFall courseFields = startDate "F" courseFields
    halfWinter courseFields = startDate "S" courseFields
    full session1 courseFields = startDate session1 courseFields

-- | Obtains the starting date for each day
startDate :: String -> [[Double]] -> [String]
startDate session dayFields = map format (generateDate session (getDay $ (concat dayFields) !! 0))

-- | Formats the date in the following way: month/day/year
format :: Day -> String
format date = formatTime defaultTimeLocale "%D" date

-- | Generates all the dates given the specific day and session
generateDate :: String -> String -> [Day]
generateDate "F" courseDay = generateDatesFall courseDay
generateDate "S" courseDay = generateDatesWinter courseDay
generateDate _ _  = []

-- | Gives the appropriate day for the course
getDay :: Double -> String 
getDay 0.0 = "M"
getDay 1.0 = "T"
getDay 2.0 = "W"
getDay 3.0 = "R"
getDay 4.0 = "F"
getDay _ = "That is not a valid representation of a day"

-- | Generate all the dates given the specific days
-- | First day of classes will be on September 14.
-- | Last day of classes will be on December 8
generateDatesFall :: String -> [Day]
generateDatesFall "M" = map (add firstMondayFall) [0,7 .. 84]
generateDatesFall "T" = map (add $ addDays 1 firstMondayFall) [0,7 .. 84]
generateDatesFall "W" = map (add $ addDays 2 firstMondayFall) [0,7 .. 77]
generateDatesFall "R" = map (add $ addDays 3 firstMondayFall) [0,7 .. 77]
generateDatesFall "F" = map (add $ addDays 4 firstMondayFall) [0,7 .. 77]
generateDatesFall _ = []

-- | Generate all the dates given the specific days
-- | First day of classes will be on January 11.
-- | Last day of classes will be on April 8
generateDatesWinter :: String -> [Day]
generateDatesWinter "M" = map (add firstMondayWinter) [0,7 .. 84]
generateDatesWinter "T" = map (add $ addDays 1 firstMondayWinter) [0,7 .. 84]
generateDatesWinter "W" = map (add $ addDays 2 firstMondayWinter) [0,7 .. 84]
generateDatesWinter "R" = map (add $ addDays 3 firstMondayWinter) [0,7 .. 84]
generateDatesWinter "F" = map (add $ addDays 4 firstMondayWinter) [0,7 .. 84]
generateDatesWinter _ = []

-- | Adds a given number of days to a given date
add :: Day -> Integer -> Day
add day num = addDays num day