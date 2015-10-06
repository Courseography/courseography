module Response.Calendar
    (calendarResponse) where

import Data.List (sort, groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Time (Day, addDays, formatTime, getCurrentTime)
import Happstack.Server (ServerPart, Response, toResponse)
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Database.CourseQueries (returnTutorial, returnLecture)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Database.Tables hiding (Session)
import Config (firstMondayFall,
               lastWednesdayFall,
               firstMondayWinter,
               lastMondayWinter,
               outDay,
               holidays)

-- | Returns an ICS file of events as requested by the user.
calendarResponse :: String -> ServerPart Response
calendarResponse courses =
    liftIO $ getCalendar courses

-- | Gets together all the pieces of the program.
getCalendar :: String -> IO Response
getCalendar courses = do
    let courseInfo = getInfoCookies courses
    databaseInfo <- mapM pullDatabase courseInfo
    currentTime <- getCurrentTime
    let systemTime = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" currentTime
    return $ toResponse $ getICS $ databaseInfo >>= getEvents systemTime

-- | A list of all the events created for a course.
type Events = [String]

-- | A string representaion for an ICS file.
type ICSFile = String

-- | Generates a string representing an ICS file.
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

-- | The code for a course.
type Code = String

-- | The section for a course.
type Section = String

-- | The session for a course.
type Session = String

-- | Obtains the code, section and session for each course in the cookies.
getInfoCookies :: String -> [(Code, Section, Session)]
getInfoCookies courses = map courseInfo allCourses
    where
        courseInfo [code, sect, session] = (code, sect, session)
        courseInfo _ = ("", "", "")
        allCourses = map (splitOn "-") (splitOn "_" courses)

-- | Pulls either a Lecture or Tutorial from the database.
pullDatabase :: (Code, Section, Session) -> IO (Maybe (Either Lecture Tutorial))
pullDatabase (code, 'L':sectCode, session) =
    fmap (fmap Left) (returnLecture (T.pack code)
                                    (T.pack $ 'L':sectCode)
                                    (T.pack session))
pullDatabase (code, sect, session) =
    fmap (fmap Right) (returnTutorial (T.pack code)
                                      (T.pack sect)
                                      (T.pack session))

-- | The current date and time as obtained from the system.
type SystemTime = String

-- | Creates all the events for a course.
getEvents :: SystemTime -> Maybe (Either Lecture Tutorial) -> Events
getEvents _ Nothing = []
getEvents systemTime (Just lect) =
    concatMap eventsByDate (zip' (third courseInfo)
                                 (fourth courseInfo)
                                 (fifth courseInfo))
    where
        eventsByDate (start, end, dates) = concatMap (formatEvents dates)
                                                     (zip start end)
        courseInfo = getCourseInfo lect
        formatEvents (startDate, endDate) (start1, end1)
            | (startDate == "" || endDate == "") = []
            | otherwise =
                ["BEGIN:VEVENT",
                 "DTSTAMP:" ++ systemTime,
                 "DTSTART;TZID=America/Toronto:" ++ startDate ++ start1,
                 "DTEND;TZID=America/Toronto:" ++ startDate ++ end1,
                 "RRULE:FREQ=WEEKLY;UNTIL=" ++ endDate ++ "000000Z"] ++
                map (\date -> "EXDATE;TZID=America/Toronto:" ++ date ++ start1)
                    holidays ++
                ["ORGANIZER:University of Toronto",
                 "SUMMARY:" ++ first courseInfo ++ " " ++ second courseInfo,
                 "CATEGORIES:EDUCATION",
                 "END:VEVENT"]

-- | A list including all the start times for a course ordered by day.
type StartTimesByDay = [[String]]

-- | A list including all the end times for a course ordered by day.
type EndTimesByDay = [[String]]

-- | A list containing all the dates for a course ordered by day.
type DatesByDay = [(StartDate, EndDate)]

-- | Obtains all the necessary information to create events for a course,
-- such as code, section, start times, end times and dates.
getCourseInfo :: Either Lecture Tutorial
              -> (Code, Section, StartTimesByDay, EndTimesByDay, DatesByDay)
getCourseInfo (Left lect) = (code, sect, start, end, dates)
    where
        code = T.unpack $ lectureCode lect
        sect = T.unpack $ lectureSection lect
        dataInOrder = orderTimeFields $ lectureTimes lect
        start = startTimesByCourse dataInOrder (T.unpack $ lectureSession lect)
        end = endTimesByCourse dataInOrder (T.unpack $ lectureSession lect)
        dates = getDatesByCourse dataInOrder (T.unpack $ lectureSession lect)
getCourseInfo (Right lect) = (code, sect, start, end, dates)
    where
        code = T.unpack $ tutorialCode lect
        sect = maybe "" T.unpack (tutorialSection lect)
        dataInOrder = orderTimeFields $ tutorialTimes lect
        start = startTimesByCourse dataInOrder (T.unpack $ tutorialSession lect)
        end = endTimesByCourse dataInOrder (T.unpack $ tutorialSession lect)
        dates = getDatesByCourse dataInOrder (T.unpack $ tutorialSession lect)

-- ** Functions that deal with tuples

-- | Join three lists together in tuples of three elements.
zip' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip' (x:xs) (y:ys) (z:zs) = (x,y,z):zip' xs ys zs
zip' _ _ _ = []

-- | Obtains the first item of the tuple, which represents the code for a course.
first :: (a, b, c, d, e) -> a
first (code, _, _, _, _) = code

-- | Obtains the second item of the tuple, which represents the section for a
-- course.
second :: (a, b, c, d, e) -> b
second (_, sect, _, _, _) = sect

-- | Obtains the third item of the tuple, which represents the start times for
-- a course.
third :: (a, b, c, d, e) -> c
third (_, _, start, _, _) = start

-- | Obtains the fourth item of the tuple, which represents the end times for a
-- course.
fourth :: (a, b, c, d, e) -> d
fourth (_, _, _, end, _) = end

-- | Obtains the fifth item of the tuple, which represents the dates for a course.
fifth :: (a, b, c, d, e) -> e
fifth (_, _, _, _, dates) = dates

-- ** Ordering data

-- | A list of the information within the time fields ordered by day.
type InfoTimeFieldsByDay = [[[Double]]]

-- | Orders by day the time fields obtained from the database.
orderTimeFields :: [Time] -> InfoTimeFieldsByDay
orderTimeFields timeFields = groupBy (\x y -> head x == head y) sortedList
    where
        sortedList = sortBy compare (map timeField timeFields)

-- ** Start time

-- | Obtains the start times for each course depending on its session.
startTimesByCourse :: InfoTimeFieldsByDay -> Session -> StartTimesByDay
startTimesByCourse dataInOrder "Y" = startTime dataInOrder ++
                                     startTime dataInOrder
startTimesByCourse dataInOrder _ = startTime dataInOrder

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

-- | Obtains the end times for each course depending on its session.
endTimesByCourse :: InfoTimeFieldsByDay -> Session -> StartTimesByDay
endTimesByCourse dataInOrder "Y" = endTime dataInOrder ++
                                   endTime dataInOrder
endTimesByCourse dataInOrder _ = endTime dataInOrder

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

-- | The string representation for times.
type TimeString = String

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

-- | The string representaion for minutes.
type MinutesString = String

-- | Creates a string for the minutes out of the decimal part given as a parameter.
formatMinutes :: Int -> MinutesString
formatMinutes decimal = if minutes >= 10 then show minutes else '0' : show minutes
    where
        minutes = decimal * 6

-- ** Start/End date

-- | Obtains all the dates for each course depending on its session.
getDatesByCourse :: InfoTimeFieldsByDay -> Session -> DatesByDay
getDatesByCourse dataInOrder session
    | session == "Y" = concat [map (getDatesByDay "F") dataInOrder ++
                               map (getDatesByDay "S") dataInOrder]
    | otherwise = concat [map (getDatesByDay session) dataInOrder]

-- | The string representation for a date in which an event
-- occurs for the first time.
type StartDate = String

-- | String representation of a date after which no more events
-- are created.
type EndDate = String

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
