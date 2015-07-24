module CalendarResponse where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Time (Day, UTCTime, addDays, formatTime, getCurrentTime, utctDay)
import Happstack.Server (ServerPart, Response, toResponse)
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Config (firstMondayFall, lastWednesdayFall ,firstMondayWinter, lastMondayWinter, outDay)
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
    currentTime <- getCurrentTime
    let timeSystem = getTime currentTime
    let events = concat $ map (getEvent timeSystem) databaseInfo
    return $ toResponse $ getCSV $ events

-- | Generates a properly formatted current date and time.
getTime :: UTCTime -> String
getTime currentTime = date ++ "T" ++ hour ++ "Z"
    where
    date = formatTime defaultTimeLocale "%Y%m%d" (utctDay currentTime)
    hour = formatTime defaultTimeLocale "%H%M%S" currentTime

-- | Generates a string representing a CSV file.
getCSV :: [String] -> String
getCSV events =  if events == [] then "" else unlines $ header ++ events ++ bottom
    where
    header = ["BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//Courseography//Calendar", "CALSCALE:GREGORIAN", "METHOD:PUBLISH"]
    bottom = ["END:VCALENDAR"]

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
getEvent :: String -> ([Time], String, String) -> [String]
getEvent timeSystem (timeFields, code, session) = concat $ eventsByCourse code session start end dates timeSystem
    where
    dataInOrder = orderData timeFields
    start = startTime dataInOrder
    end = endTime dataInOrder
    dates = getDates session dataInOrder

-- | Creates an event for each course
eventsByCourse :: String -> String -> [[String]] -> [[String]] -> [[(String, String)]] -> String -> [[String]]
eventsByCourse code session start end dates timeSystem = map (eventsByDay code session timeSystem) (zip' start end dates)

-- | Join three lists together in tuples of three elements
zip' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip' _ _ [] = []
zip' _ [] _ = []
zip' [] _ _ = []
zip' (x:xs) (y2:ys) (z:zs) = (x,y2,z):zip' xs ys zs

-- | Creates an event for each day
eventsByDay :: String -> String -> String -> ([String], [String], [(String, String)]) -> [String]
eventsByDay code session timeSystem (start, end, dates) = if session == "Y" then year else half
    where
    year = halfFall ++ halfWinter
    halfFall = eventsByTime code start end (dates !! 0) timeSystem
    halfWinter = eventsByTime code start end (dates !! 1) timeSystem
    half = eventsByTime code start end (dates !! 0) timeSystem

-- | Creates an event for each start/end time
eventsByTime :: String -> [String] -> [String] -> (String, String) -> String -> [String]
eventsByTime code start end date timeSystem = 
    if fst date == "" || snd date == "" then []
    else map (eventsByDate code date timeSystem) (zip start end)

-- | Generates the string that represents the event for each course
eventsByDate :: String -> (String, String) -> String -> (String, String) -> String
eventsByDate code (startDate, endDate) timeSystem (start, end) = 
    "BEGIN:VEVENT\n" ++ "DTSTAMP:" ++ timeSystem ++ 
    "\nDTSTART;TZID=America/Toronto:" ++ startDate ++ start ++ "\nDTEND;TZID=America/Toronto:" ++
    startDate ++ end ++ "\nRRULE:FREQ=WEEKLY;UNTIL=" ++ endDate ++ "000000Z" ++ "\nORGANIZER:U of T" ++
    "\nSUMMARY:" ++ code ++ "\nCATEGORIES:SCHOOL\n" ++ "END:VEVENT"

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

-- | Creates the string time in the following way HourMinutesSeccondsZ.
-- For instance, 133500Z corresponds to 1:35 pm.
getStr :: Double -> String
getStr fullTime = if minutes == 0
                  then hour ++ "0000"
                  else hour ++ ratio minutes ++ "00"
    where
    hours = splitOn "." (show fullTime)
    hour = hours !! 0
    minutes = read (hours !! 1) :: Int

-- | Gets the minutes out of a decimal part of my time
ratio :: Int -> String
ratio decimal = if minutes >= 10 then show minutes else "0" ++ show minutes
    where
    minutes = decimal * 6

-- ** Start/End date

-- | Obtains all the dates for each course depending on its session
getDates :: String -> [[[Double]]] -> [[(String, String)]]
getDates session dataInOrder = map (checkSession session) dataInOrder
    where
    checkSession session1 courseFields = if session1 == "Y" 
                                        then [halfFall courseFields, halfWinter courseFields]
                                        else [full session1 courseFields]
    halfFall courseFields = getDate "F" courseFields
    halfWinter courseFields = getDate "S" courseFields
    full session1 courseFields = getDate session1 courseFields

-- | Gives the appropiate starting date for the course
-- | Obtains the starting date for each day
getDate :: String -> [[Double]] -> (String, String)
getDate session dayFields = if session == "F" then getFallStr else getWinterStr
    where
    getFallStr = format $ getDayFall $ (concat dayFields) !! 0
    getWinterStr = format $ getDayWinter $ (concat dayFields) !! 0

-- | Formats the date in the following way: YearMonthDayT.
-- For instance, 20150720T corresponds to July 20th, 2015. 

format :: (Day, Day) -> (String, String)
format (start, end) = if start == outDay || end == outDay then ("", "")else (startStr , endStr)
    where
    startStr = formatTime defaultTimeLocale "%Y%m%d" start ++ "T"
    endStr = formatTime defaultTimeLocale "%Y%m%d" end ++ "T"

-- | Gives the appropriate day for courses in the Fall
getDayFall :: Double -> (Day, Day) 
getDayFall 0.0 = (firstMondayFall, addDays 5 lastWednesdayFall)
getDayFall 1.0 = (addDays 1 firstMondayFall, addDays 6 lastWednesdayFall)
getDayFall 2.0 = (addDays 2 firstMondayFall, lastWednesdayFall)
getDayFall 3.0 = (addDays 3 firstMondayFall, addDays 1 lastWednesdayFall)
getDayFall 4.0 = (addDays 4 firstMondayFall, addDays 2 lastWednesdayFall)
getDayFall _ = (outDay, outDay)

-- | Gives the appropriate day for courses in the Winter
getDayWinter :: Double -> (Day, Day) 
getDayWinter 0.0 = (firstMondayWinter, lastMondayWinter)
getDayWinter 1.0 = (addDays 1 firstMondayWinter, addDays 1 lastMondayWinter)
getDayWinter 2.0 = (addDays 2 firstMondayWinter, addDays 2 lastMondayWinter)
getDayWinter 3.0 = (addDays 3 firstMondayWinter, addDays 3 lastMondayWinter)
getDayWinter 4.0 = (addDays 4 firstMondayWinter, addDays 4 lastMondayWinter)
getDayWinter _ = (outDay, outDay)