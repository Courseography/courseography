module CalendarResponse where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Time (Day, UTCTime, addDays, formatTime, getCurrentTime)
import Happstack.Server (ServerPart, Response, toResponse)
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Database.CourseQueries (returnTutorialTimes, returnLectureTimes)
import qualified Data.Text as T
import Database.Tables as Tables
import Config (firstMondayFall,
               lastWednesdayFall,
               firstMondayWinter,
               lastMondayWinter,
               outDay)

-- | Returns a CSV file of events as requested by the user.
calendarResponse :: String -> ServerPart Response
calendarResponse courses =
    liftIO $ getCalendar courses

-- | Gets together all the pieces of the program.
getCalendar :: String -> IO Response
getCalendar courses = do
    let courseInfo = getCoursesInfo courses
    databaseInfo <- mapM pullDatabase courseInfo
    currentTime <- getCurrentTime
    let timeSystem = getTime currentTime
    let events = concat $ map (getEvent timeSystem) databaseInfo
    return $ toResponse $ getCSV events

-- | Generates a properly formatted current date and time.
getTime :: UTCTime -> String
getTime currentTime = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" currentTime

-- | Generates a string representing a CSV file.
getCSV :: [String] -> String
getCSV events =
    if null events
    then ""
    else unlines $ header ++ events ++ bottom
    where
        header = ["BEGIN:VCALENDAR", "VERSION:2.0",
                  "PRODID:-//Courseography//Calendar",
                  "CALSCALE:GREGORIAN", "METHOD:PUBLISH"]
        bottom = ["END:VCALENDAR"]

-- ** Cookie Information

-- | Obtains a list with all the information about the courses obtained from 
-- the cookies.
getCoursesInfo :: String -> [(String, String, String)]
getCoursesInfo courses = map courseInfo allCourses
    where
        courseInfo course = (course !! 0, course !! 1, course !! 2)
        allCourses = map (splitOn "-") byCourse
        byCourse = splitOn "_" courses

-- ** Database Information

-- | Pulls out the information for each course from the database.
pullDatabase :: (String, String, String) -> IO ([Time], String, String, String)
pullDatabase (code, sect, session) =
    if take 1 sect == "L"
    then getData $ returnLectureTimes (T.pack code)
                                      (T.pack sect)
                                      (T.pack session)
    else getData $ returnTutorialTimes (T.pack code)
                                       (T.pack sect)
                                       (T.pack session)
    where
        getData info = do
            courseInfo <- info 
            return (courseInfo, code, session, sect)

-- ** Event Creation

-- | Generates an event for each course.
getEvent :: String -> ([Time], String, String, String) -> [String]
getEvent timeSystem (timeFields, code, session, sect) = 
    concat $ eventsByCourse code session sect start end dates timeSystem
    where
        dataInOrder = orderData timeFields
        start = startTime dataInOrder
        end = endTime dataInOrder
        dates = getDatesByCourse session dataInOrder

-- | Creates an event for each course depending on its session.
eventsByCourse :: String -- ^ Course code.
            -> String -- ^ Course session.
            -> String -- ^ Course section.
            -> [[String]] -- ^ Start times.
            -> [[String]] -- ^ End times.
            -> [[(String, String)]] -- ^ Start/End dates.
            -> String -- ^ Current time.
            -> [[String]]
eventsByCourse code session sect start end dates timeSystem =
    if session == "Y" then year else halfCourse
    where
        year = halfFall ++ halfWinter
        halfFall = eventsByTime code sect start end (dates !! 0) timeSystem
        halfWinter = eventsByTime code sect start end (dates !! 1) timeSystem
        halfCourse = eventsByTime code sect start end (dates !! 0) timeSystem

-- | Creates an event for each start/end time and date.
eventsByTime :: String -- ^ Course code.
             -> String  -- ^ Course section.
             -> [[String]] -- ^ Start times.
             -> [[String]] -- ^ End times.
             -> [(String, String)] -- ^ Start/End dates.
             -> String -- ^ Current time.
             -> [[String]]
eventsByTime code sect start end dates timeSystem =
    map (eventsByDate code sect timeSystem) (zip' start end dates)

-- | Join three lists together in tuples of three elements
zip' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip' _ _ [] = []
zip' _ [] _ = []
zip' [] _ _ = []
zip' (x:xs) (y2:ys) (z:zs) = (x,y2,z):zip' xs ys zs

-- | Creates an event for each start/end time.
eventsByDate :: String -- ^ Course code.
             -> String -- ^ Course section.
             -> String -- ^ Current time.
             -> ([String], -- ^ Start time
             [String], -- ^ End time
             (String, String)) -- ^ Start/End date.
             -> [String]
eventsByDate code sect timeSystem (start, end, dates) =
    concat $ map (eventsGenerator code sect timeSystem dates) (zip start end)

-- | Generates the string that represents the event for each course.
eventsGenerator :: String -- ^ Course code.
                -> String -- ^ Course section.
                -> String -- ^ Current time.
                -> (String, String) -- ^ Start/End date.
                -> (String, String) -- ^ Start/End time.
                -> [String]
eventsGenerator code sect timeSystem (startDate, endDate) (start, end) =
    if startDate == "" || endDate == ""
    then []
    else ["BEGIN:VEVENT", "DTSTAMP:" ++ timeSystem,
          "DTSTART;TZID=America/Toronto:" ++ startDate ++ start,
          "DTEND;TZID=America/Toronto:" ++ startDate ++ end,
          "RRULE:FREQ=WEEKLY;UNTIL=" ++ endDate ++ "000000Z",
          "ORGANIZER:University of Toronto",
          "SUMMARY:" ++ code ++ " " ++ sect,
          "CATEGORIES:EDUCATION", "END:VEVENT"]

-- ** Ordering data

-- | Orders the time fields for each course; which were obtained from the
-- database
orderData :: [Time] -> [[[Double]]]
orderData courseFields = filter notNull (assignDay $ map timeField courseFields)
    where
        notNull field = not $ null field

-- | Organizes the data by the day of the week, assigning a position based on
-- the day in the time field.
assignDay :: [[Double]] -> [[[Double]]]
assignDay lst = [monday, tuesday, wednesday, thursday, friday]
    where
        monday = filter (\ field -> field !! 0 == 0.0) lst
        tuesday = filter (\ field -> field !! 0 == 1.0) lst
        wednesday = filter (\field -> field !! 0 == 2.0) lst
        thursday = filter (\field -> field !! 0 == 3.0) lst
        friday = filter (\field -> field !! 0 == 4.0) lst

-- ** Start time

-- | Obtains a list of start times for each course.
startTime :: [[[Double]]] -> [[String]]
startTime dataInOrder = map checkTimeStart dataInOrder
    where
        checkTimeStart dataByDay = 
            map getStrTime ([head (sortList dataByDay)] ++
                        getStartConsecutives (sortList dataByDay))
        sortList dataDay = sort $ map (!! 1) dataDay

-- | Gets the start times that are not the very first start time. 
getStartConsecutives :: [Double] -> [Double]
getStartConsecutives lst = 
    filter (/= 30.0) ([if lst !! i == lst !! (i + 1) - 0.5
                       then 30.0
                       else lst !! (i + 1)| i <- [0 .. l]])
    where
        l = length lst - 2

-- ** End time

-- | Obtains a list of end times for each course.
endTime :: [[[Double]]] -> [[String]]
endTime dataInOrder = map checkTimeEnd dataInOrder
    where
        checkTimeEnd dataByDay =
            map (getStrTime . (+ 0.5))
                (getEndConsecutives (sortList dataByDay) ++
                                     [last (sortList dataByDay)])
        sortList dataDay = sort $ map (!! 1) dataDay

-- | Gets the end times that are not the very first end time. 
getEndConsecutives :: [Double] -> [Double]
getEndConsecutives lst =
    filter (/= 30.0) ([if lst !! i == lst !! (i + 1) - 0.5
                       then 30.0
                       else lst !! i| i <- [0 .. l]])
    where
        l = length lst - 2

-- ** Function that works for both start and end times

-- | Creates a string time in the following way HourMinutesSeccondsZ.
-- For instance, 133500Z corresponds to 1:35 pm.
getStrTime :: Double -> String
getStrTime fullTime = if minutes == 0
                  then hour ++ "0000"
                  else hour ++ ratio minutes ++ "00"
    where
        hours = splitOn "." (show fullTime)
        hour = hours !! 0
        minutes = read $ hours !! 1 :: Int

-- | Creates a string for the minutes.
ratio :: Int -> String
ratio decimal = if minutes >= 10 then show minutes else '0' : show minutes
    where
        minutes = decimal * 6

-- ** Start/End date

-- | Obtains all the dates for each course depending on its session.
getDatesByCourse :: String -- ^ Course session.
                 -> [[[Double]]] -- ^ Time fields for the week ordered by day.
                 -> [[(String, String)]]
getDatesByCourse session dataInOrder =
    if session == "Y"
    then [map halfFall dataInOrder, map halfWinter dataInOrder]
    else [map (halfYearCourse session) dataInOrder]
    where
        halfFall dataByDay = getDatesByDay "F" dataByDay
        halfWinter dataByDay = getDatesByDay "S" dataByDay
        halfYearCourse session1 dataByDay = getDatesByDay session1 dataByDay

-- | Gives the appropiate starting and ending dates for each day,in which the
-- course takes place, depending on the course session.
getDatesByDay :: String -- ^ Course session.
             -> [[Double]] -- ^ Course session.
             -> (String, String) -- ^ Time fields for only one day of the week.
getDatesByDay session dataByDay =
    if session == "F" then getFallStr else getWinterStr
    where
        getFallStr = format $ getDayFall $ concat dataByDay !! 0
        getWinterStr = format $ getDayWinter $ concat dataByDay !! 0

-- | Formats the date in the following way: YearMonthDayT.
-- For instance, 20150720T corresponds to July 20th, 2015. 
format :: (Day, Day) -> (String, String)
format (start, end) = 
    if start == outDay || end == outDay
    then ("", "")
    else (startStr , endStr)
    where
        startStr = formatTime defaultTimeLocale "%Y%m%dT" start
        endStr = formatTime defaultTimeLocale "%Y%m%dT" end

-- | Gives the appropriate starting and ending dates for courses in the Fall.
getDayFall :: Double -> (Day, Day) 
getDayFall 0.0 = (firstMondayFall, addDays 6 lastWednesdayFall)
getDayFall 1.0 = (addDays 1 firstMondayFall, addDays 7 lastWednesdayFall)
getDayFall 2.0 = (addDays 2 firstMondayFall, addDays 1 lastWednesdayFall)
getDayFall 3.0 = (addDays 3 firstMondayFall, addDays 2 lastWednesdayFall)
getDayFall 4.0 = (addDays 4 firstMondayFall, addDays 3 lastWednesdayFall)
getDayFall _ = (outDay, outDay)

-- | Gives the appropriate starting and ending dates for courses in the Winter.
getDayWinter :: Double -> (Day, Day) 
getDayWinter 0.0 = (firstMondayWinter, addDays 1 lastMondayWinter)
getDayWinter 1.0 = (addDays 1 firstMondayWinter, addDays 2 lastMondayWinter)
getDayWinter 2.0 = (addDays 2 firstMondayWinter, addDays 3 lastMondayWinter)
getDayWinter 3.0 = (addDays 3 firstMondayWinter, addDays 4 lastMondayWinter)
getDayWinter 4.0 = (addDays 4 firstMondayWinter, addDays 5 lastMondayWinter)
getDayWinter _ = (outDay, outDay)
