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


-- | A list of all the events created for a course.
type Events = [String]

-- | A string representaion for an ICS file.
type ICSFile = String

-- | All the information for a course, including the information from
-- the cookies; as well as, the start and end times, and the dates.
type CourseInformation =
    (Code, Section, Session, StartTimesByDay, EndTimesByDay, DatesByDay)

-- | The code for a course.
type Code = String

-- | The section for a course.
type Section = String

-- | The session for a course.
type Session = String

-- | A list including all the start times for a course ordered by day.
type StartTimesByDay = [[String]]

-- | A list including all the end times for a course ordered by day.
type EndTimesByDay = [[String]]

-- | A list containing all the dates for a course ordered by day.
type DatesByDay = [[(StartDate, EndDate)]]

-- | The string representation for a date in which an event
-- occurs for the first time.
type StartDate = String

-- | String representation of a date after which no more events
-- are created.
type EndDate = String

-- | A list of the information within the time fields ordered by day.
type InfoTimeFieldsByDay = [[[Double]]]

-- | List that contains string representations of the fall and
-- winter holidays.
type Holidays = [String]

-- | The current date and time as obtained from the system.
type SystemTime = String

-- | The string representation for times 
type TimeString = String

-- | The string representaion for minutes
type MinutesString = String

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
    return $ toResponse $ getICS $ databaseInfo >>= getEvents systemTime


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
getDataCourse code sect session timeFields = (code, sect, session, start, end, dates)
    where
        dataInOrder = orderTimeFields timeFields
        start = startTime dataInOrder
        end = endTime dataInOrder
        dates = getDatesByCourse session dataInOrder

-- ** Event Creation

-- | Generates an event for each course depending on its session.
getEvents :: SystemTime -> CourseInformation -> Events
getEvents systemTime (code, sect, "Y", start, end, dates) =
    eventsByTime code sect start end systemTime fallHolidays (dates !! 0) ++
    eventsByTime code sect start end systemTime winterHolidays (dates !! 1)
getEvents systemTime (code, sect, "F", start, end, dates) =
    eventsByTime code sect start end systemTime fallHolidays (dates !! 0)
getEvents systemTime (code, sect, _, start, end, dates) =
    eventsByTime code sect start end systemTime winterHolidays (dates !! 0)

-- | Creates an event for each course using each start/end time and date.
eventsByCourse :: Code
               -> Section
               -> StartTimesByDay
               -> EndTimesByDay
               -> SystemTime
               -> Holidays
               -> [(String, String)] -- ^ Start/End dates by day.
               -> Events
eventsByCourse code sect start end systemTime holidays dates =
    concatMap eventsByDate (zip' start end dates)
    where
        eventsByDate (start1, end1, dates1) = concatMap (formatEvents dates1)
                                                        (zip start1 end1)
        formatEvents (startDate, endDate) (start2, end2)
          | (startDate == "" || endDate == "") = []
          | otherwise =
              ["BEGIN:VEVENT",
               "DTSTAMP:" ++ systemTime,
               "DTSTART;TZID=America/Toronto:" ++ startDate ++ start2,
               "DTEND;TZID=America/Toronto:" ++ startDate ++ end2,
               "RRULE:FREQ=WEEKLY;UNTIL=" ++ endDate ++ "000000Z"] ++
              map (\date -> "EXDATE;TZID=America/Toronto:" ++ date ++ start2)
                  holidays ++
              ["ORGANIZER:University of Toronto",
               "SUMMARY:" ++ code ++ " " ++ sect,
               "CATEGORIES:EDUCATION",
               "END:VEVENT"]

-- | Join three lists together in tuples of three elements
zip' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip' (x:xs) (y:ys) (z:zs) = (x,y,z):zip' xs ys zs
zip' _ _ _ = []

-- ** Ordering data

-- | Orders the time fields for each course; which were obtained from the
-- database
orderTimeFields :: [Time] -> InfoTimeFieldsByDay
orderTimeFields timeFields = filter (not . null)
                                (assignDay $ map timeField timeFields)

-- | Organizes the data by the day of the week, assigning a position based on
-- the day in the time field.
assignDay :: [[Double]] -> InfoTimeFieldsByDay
assignDay lst = groupBy (\x y -> head x == head y) sortedList
    where
        sortedList = sortBy compare lst

-- ** Start time

-- | Obtains the start times for each course by day.
startTime :: InfoTimeFieldsByDay -> StartTimesByDay
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
endTime :: InfoTimeFieldsByDay -> EndTimesByDay
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
getDatesByCourse :: Session -> InfoTimeFieldsByDay -> DatesByDay
getDatesByCourse session dataInOrder
    | session == "Y" = [map (getDatesByDay "F") dataInOrder,
                        map (getDatesByDay "S") dataInOrder]
    | otherwise = [map (getDatesByDay session) dataInOrder]

-- | Gives the appropiate starting and ending dates for each day,in which the
-- course takes place, depending on the course session.
getDatesByDay :: Session -> [[Double]] -> (StartDate, EndDate)
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
