module CalendarResponse where

import Data.List (sort, groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Time (Day, addDays, formatTime, getCurrentTime)
import Happstack.Server (ServerPart, Response, toResponse)
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Database.CourseQueries (returnTutorialTimes, returnLectureTimes)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Database.Tables (Time, timeField)
import Config (firstMondayFall,
               lastWednesdayFall,
               firstMondayWinter,
               lastMondayWinter,
               outDay,
               fallHolidays,
               winterHolidays)

-- | A string representaion for an ICS file.
type ICSFile = String

-- | A list of all the events created for all courses.
type Events = [String]

-- | All the information for a course, including the
-- time fields obtained from the database and the
-- information from the cookies.
type CourseInformation = (TimeFields, Code, Section, Session)

-- | A list of all the time fields for a course as obtained
-- from the database.
type TimeFields = [Time]

-- | The code for a course.
type Code = String

-- | The section for a course.
type Section = String

-- | The session for a course.
type Session = String

-- | The current date and time as obtained from the system.
type SystemTime = String

-- | A list of the information within the time fields
-- ordered by day.
type TimeFieldsByDay = [[TimeFieldInfo]]

-- | The information contained in a time Field. The first
-- Double represents the day and the second one the time.
type TimeFieldInfo = [Double]

-- | A list including all the times for a course ordered
-- by day.
type TimesByDay = [[String]]

-- | The string representation for times 
type TimeString = String

-- | The string representaion for minutes
type MinutesString = String

-- | A list containing all the dates for an event
type DatesByEvent = [[(StartDate, EndDate)]]

-- | The string representation for a date in which an event
-- occurs for the first time.
type StartDate = String

-- | String representation of a date after which no more events
-- are created.
type EndDate = String

-- | Returns an ICS file of events as requested by the user.
calendarResponse :: String -> ServerPart Response
calendarResponse courses =
    liftIO $ getCalendar courses

-- | Gets together all the pieces of the program.
getCalendar :: String -> IO Response
getCalendar courses = do
    let courseInfo = getCoursesInfo courses
    databaseInfo <- mapM pullDatabase courseInfo
    currentTime <- getCurrentTime
    let systemTime = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" currentTime
    let events = databaseInfo >>= getEvents systemTime
    return $ toResponse $ getICS events

-- | Generates a string representing a ICS file.
getICS :: Events -> ICSFile
getICS [] = ""
getICS events = unlines $ header ++ events ++ bottom
    where
        header = ["BEGIN:VCALENDAR",
                  "VERSION:2.0",
                  "PRODID:-//Courseography//Calendar",
                  "CALSCALE:GREGORIAN",
                  "METHOD:PUBLISH"]
        bottom = ["END:VCALENDAR"]

-- ** Cookie Information

-- | Obtains a list with all the information about the courses obtained from
-- the cookies.
getCoursesInfo :: String -> [(Code, Section, Session)]
getCoursesInfo courses = map courseInfo allCourses
    where
        courseInfo [code, sect, session] = (code, sect, session)
        courseInfo _ = ("", "", "")
        allCourses = map (splitOn "-") (splitOn "_" courses)

-- ** Database Information

-- | Pulls out the information for each course from the database.
pullDatabase :: (Code, Section, Session) -> IO CourseInformation
pullDatabase (code, 'L':sectCode, session) =
    fmap (getDataCourse code ('L':sectCode) session)
         (returnLectureTimes (T.pack code)
                             (T.pack $ 'L':sectCode)
                             (T.pack session))
pullDatabase (code, sect, session) =
    fmap (getDataCourse code sect session)
         (returnTutorialTimes (T.pack code)
                              (T.pack sect)
                              (T.pack session))

-- | Returns a tuple with a combination of the information needed from both
-- the database and the cookies.
getDataCourse :: Code -> Section -> Session -> TimeFields -> CourseInformation
getDataCourse code sect session timeFields = (timeFields, code, sect, session)

-- ** Event Creation

-- | Generates an event for each course.
getEvents :: SystemTime -> CourseInformation -> Events
getEvents systemTime (timeFields, code, sect, session) =
    concat $ eventsByCourse code sect session start end dates systemTime
    where
        dataInOrder = orderData timeFields
        start = startTime dataInOrder
        end = endTime dataInOrder
        dates = getDatesByCourse session dataInOrder

-- | Creates an event for each course depending on its session.
eventsByCourse :: String -- ^ Course code.
               -> String -- ^ Course section.
               -> String -- ^ Course session.
               -> [[String]] -- ^ Start times.
               -> [[String]] -- ^ End times.
               -> [[(String, String)]] -- ^ Start/End dates.
               -> String -- ^ Current system time.
               -> [[String]]
eventsByCourse code sect "Y" start end dates systemTime =
    eventsByTime code sect start end (dates !! 0) systemTime fallHolidays ++
    eventsByTime code sect start end (dates !! 1) systemTime winterHolidays
eventsByCourse code sect "F" start end dates systemTime =
    eventsByTime code sect start end (dates !! 0) systemTime fallHolidays
eventsByCourse code sect _ start end dates systemTime =
    eventsByTime code sect start end (dates !! 0) systemTime winterHolidays

-- | Creates an event for each start/end time and date.
eventsByTime :: String -- ^ Course code.
             -> String -- ^ Course section.
             -> [[String]] -- ^ Start times.
             -> [[String]] -- ^ End times.
             -> [(String, String)] -- ^ Start/End dates.
             -> String -- ^ Current system time.
             -> [String] -- ^ Holidays dates.
             -> [[String]]
eventsByTime code sect start end dates systemTime holidays =
    map (eventsByDate code sect systemTime holidays)
        (zip' start end dates)

-- | Join three lists together in tuples of three elements
zip' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip' (x:xs) (y:ys) (z:zs) = (x,y,z):zip' xs ys zs
zip' _ _ _ = []

-- | Creates an event for each start/end time.
eventsByDate :: String -- ^ Course code.
             -> String -- ^ Course section.
             -> String -- ^ Current system time.
             -> [String] -- ^ Holidays dates.
             -> ([String], -- ^ Start time
                 [String], -- ^ End time
                 (String, String)) -- ^ Start/End date.
             -> [String]
eventsByDate code sect systemTime holidays (start, end, dates) =
    concatMap (eventsStr code sect systemTime dates holidays) (zip start end)

-- | Generates the string that represents each event.
eventsStr :: String -- ^ Course code.
          -> String -- ^ Course section.
          -> String -- ^ Current system time.
          -> (String, String) -- ^ Start/End date.
          -> [String] -- ^ Holidays dates.
          -> (String, String) -- ^ Start/End time.
          -> [String]
eventsStr code sect systemTime (startDate, endDate) holidays (start, end)
    | (startDate == "" || endDate == "") = []
    | otherwise =
        ["BEGIN:VEVENT",
         "DTSTAMP:" ++ systemTime,
         "DTSTART;TZID=America/Toronto:" ++ startDate ++ start,
         "DTEND;TZID=America/Toronto:" ++ startDate ++ end,
         "RRULE:FREQ=WEEKLY;UNTIL=" ++ endDate ++ "000000Z"] ++
        map (\date -> "EXDATE;TZID=America/Toronto:" ++ date ++ start)
            holidays ++
        ["ORGANIZER:University of Toronto",
         "SUMMARY:" ++ code ++ " " ++ sect,
         "CATEGORIES:EDUCATION",
         "END:VEVENT"]

-- ** Ordering data

-- | Orders the time fields for each course; which were obtained from the
-- database
orderData :: TimeFields -> TimeFieldsByDay
orderData timeFields = filter (not . null)
                                (assignDay $ map timeField timeFields)

-- | Organizes the data by the day of the week, assigning a position based on
-- the day in the time field.
assignDay :: [TimeFieldInfo] -> TimeFieldsByDay
assignDay lst = groupBy (\x y -> head x == head y) sortedList
    where
        sortedList = sortBy compare lst

-- ** Start time

-- | Obtains the start times for each course by day.
startTime :: TimeFieldsByDay -> TimesByDay
startTime dataInOrder =
    map (\dataByDay -> map formatTimes
                           (head (timesOrdered dataByDay) :
                                   getStartConsecutives (timesOrdered dataByDay)))
        dataInOrder
    where
        timesOrdered dataDay = sort $ map (!! 1) dataDay

-- | Gets the start times that are not the very first start time.
getStartConsecutives :: [Double] -> [Double]
getStartConsecutives listTimes =
    filter (/= 30.0) [if listTimes !! i == listTimes !! (i + 1) - 0.5
                      then 30.0
                      else listTimes !! (i + 1)| i <- [0 .. lenForComparison]]
    where
        lenForComparison = length listTimes - 2

-- ** End time

-- | Obtains the end times for each course by day.
endTime :: TimeFieldsByDay -> TimesByDay
endTime dataInOrder =
    map (\dataByDay -> map (formatTimes . (+ 0.5))
                           (getEndConsecutives $ timesOrdered dataByDay ++
                            [last $ timesOrdered dataByDay]))
        dataInOrder
    where
        timesOrdered dataDay = sort $ map (!! 1) dataDay

-- | Gets the end times that are not the very first end time.
getEndConsecutives :: [Double] -> [Double]
getEndConsecutives listTimes =
    filter (/= 30.0) [if listTimes !! i == listTimes !! (i + 1) - 0.5
                      then 30.0
                      else listTimes !! i| i <- [0 .. lenForComparison]]
    where
        lenForComparison = length listTimes - 2

-- ** Functions that work for both start and end times

-- | Creates a string time in the following way HourMinutesSeccondsZ.
-- For instance, 133500 corresponds to 1:35 pm.
formatTimes :: Double -> TimeString
formatTimes fullTime =
    if maybe True (== 0) minutes
    then hour ++ "0000"
    else hour ++ maybe "0000" formatMinutes minutes ++ "00"
    where
        hours = splitOn "." (show fullTime)
        hour = hours !! 0
        minutes = readMaybe $ hours !! 1

-- | Creates a string for the minutes out of the decimal part given as a parameter.
formatMinutes :: Int -> MinutesString
formatMinutes decimal = if minutes >= 10 then show minutes else '0' : show minutes
    where
        minutes = decimal * 6

-- ** Start/End date

-- | Obtains all the dates for each course depending on its session.
getDatesByCourse :: Session -> TimeFieldsByDay -> DatesByEvent
getDatesByCourse session dataInOrder
    | session == "Y" = [map (getDatesByDay "F") dataInOrder,
                        map (getDatesByDay "S") dataInOrder]
    | otherwise = [map (getDatesByDay session) dataInOrder]

-- | Gives the appropiate starting and ending dates for each day,in which the
-- course takes place, depending on the course session.
getDatesByDay :: Session -> [TimeFieldInfo] -> (StartDate, EndDate)
getDatesByDay session dataByDay
    | session ==  "F" = formatDates $ getDatesFall $ head $ concat dataByDay
    | otherwise = formatDates $ getDatesWinter $ head $ concat dataByDay

-- | Formats the date in the following way: YearMonthDayT.
-- For instance, 20150720T corresponds to July 20th, 2015.
formatDates :: (Day, Day) -> (StartDate, EndDate)
formatDates (start, end)
    | start == outDay || end == outDay = ("", "")
    | otherwise = (startStr , endStr)
    where
        startStr = formatTime defaultTimeLocale "%Y%m%dT" start
        endStr = formatTime defaultTimeLocale "%Y%m%dT" end

-- | Gives the appropriate starting and ending dates for courses in the Fall.
getDatesFall :: Double -> (Day, Day)
getDatesFall 0.0 = (firstMondayFall, addDays 6 lastWednesdayFall)
getDatesFall 1.0 = (addDays 1 firstMondayFall, addDays 7 lastWednesdayFall)
getDatesFall 2.0 = (addDays 2 firstMondayFall, addDays 1 lastWednesdayFall)
getDatesFall 3.0 = (addDays 3 firstMondayFall, addDays 2 lastWednesdayFall)
getDatesFall 4.0 = (addDays 4 firstMondayFall, addDays 3 lastWednesdayFall)
getDatesFall _ = (outDay, outDay)

-- | Gives the appropriate starting and ending dates for courses in the Winter.
getDatesWinter :: Double -> (Day, Day)
getDatesWinter 0.0 = (firstMondayWinter, addDays 1 lastMondayWinter)
getDatesWinter 1.0 = (addDays 1 firstMondayWinter, addDays 2 lastMondayWinter)
getDatesWinter 2.0 = (addDays 2 firstMondayWinter, addDays 3 lastMondayWinter)
getDatesWinter 3.0 = (addDays 3 firstMondayWinter, addDays 4 lastMondayWinter)
getDatesWinter 4.0 = (addDays 4 firstMondayWinter, addDays 5 lastMondayWinter)
getDatesWinter _ = (outDay, outDay)
