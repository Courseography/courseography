module Response.Calendar
    (calendarResponse) where

import Data.List (sort, groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Time (Day, addDays, formatTime, getCurrentTime, defaultTimeLocale)
import Happstack.Server (ServerPart, Response, toResponse)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite (runSqlite, (==.), entityVal, selectList, entityKey)
import Database.CourseQueries (returnMeeting, buildTimes')
import qualified Data.Text as T
import Text.Read (readMaybe)
import Database.Tables
import Config (firstMondayFall,
               lastWednesdayFall,
               firstMondayWinter,
               lastMondayWinter,
               outDay,
               holidays,
               databasePath)

-- | Returns an ICS file of events as requested by the user.
calendarResponse :: String -> ServerPart Response
calendarResponse = liftIO . getCalendar . T.pack

-- | Gets together all the pieces of the program.
getCalendar :: T.Text -> IO Response
getCalendar courses = do
    let courseInfo = getCoursesInfo courses
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
type Code = T.Text

-- | The section for a course.
type Section = T.Text

-- | The session for a course.
type Session = T.Text

-- | Obtains the code, section and session for each course in the local storage.
getCoursesInfo :: T.Text -> [(Code, Section, Session)]
getCoursesInfo courses = map courseInfo allCourses
    where
        courseInfo [code, sect, session] = (code, sect, session)
        courseInfo _ = ("", "", "")
        allCourses = map (T.splitOn "-") (T.splitOn "_" courses)

-- | Pulls either a Lecture, Tutorial or Pratical from the database.
pullDatabase :: (Code, Section, Session) -> IO (MeetTime)
pullDatabase (code, section, session) = runSqlite databasePath $ do
    meet <- returnMeeting code fullSection session
    allTimes <- selectList [TimesMeeting ==. entityKey meet] []
    let parsedTime = map (buildTimes' . entityVal) allTimes
    return $ MeetTime {meetData = entityVal meet, timeData = parsedTime}
    where
    fullSection
        | T.isPrefixOf "L" section = T.append "LEC" sectCode
        | T.isPrefixOf "T" section = T.append "TUT" sectCode
        | T.isPrefixOf "P" section = T.append "PRA" sectCode
        | otherwise                = section
    sectCode = T.tail section

-- | The current date and time as obtained from the system.
type SystemTime = String

-- | Creates all the events for a course.
getEvents :: SystemTime -> MeetTime -> Events
getEvents systemTime lect =
    concatMap eventsByDate (zip' (third courseInfo)
                                 (fourth courseInfo)
                                 (fifth courseInfo))
    where
        eventsByDate (start, end, dates) = concatMap (formatEvents dates)
                                                     (zip start end)
        courseInfo = getCourseInfo lect
        formatEvents (startDate, endDate) (start1, end1)
            | startDate == "" || endDate == "" = []
            | otherwise =
                ["BEGIN:VEVENT",
                 "DTSTAMP:" ++ systemTime,
                 "DTSTART;TZID=America/Toronto:" ++ startDate ++ start1,
                 "DTEND;TZID=America/Toronto:" ++ startDate ++ end1,
                 "RRULE:FREQ=WEEKLY;UNTIL=" ++ endDate ++ "000000Z"] ++
                map (\date -> "EXDATE;TZID=America/Toronto:" ++ date ++ start1)
                    holidays ++
                ["ORGANIZER:University of Toronto",
                 "SUMMARY:" ++ T.unpack (first courseInfo) ++ " " ++ T.unpack (second courseInfo),
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
getCourseInfo :: MeetTime -> (Code, Section, StartTimesByDay, EndTimesByDay, DatesByDay)
getCourseInfo meeting =
    let meet = meetData meeting
        allTimes = timeData meeting
        code = meetingCode meet
        sect = meetingSection meet
        dataInOrder = orderTimeFields allTimes
        start = startTimesByCourse dataInOrder (meetingSession meet)
        end = endTimesByCourse dataInOrder (meetingSession meet)
        dates = getDatesByCourse dataInOrder (meetingSession meet)
    in (code, sect, start, end, dates)

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
type InfoTimeFieldsByDay = [[Times']]

-- | Orders by day the start and endtimes obtained from the database.
orderTimeFields :: [Times'] -> InfoTimeFieldsByDay
orderTimeFields timeFields = groupBy (\x y -> weekDay x == weekDay y) sortedList
    where
        sortedList = sortBy (comparing weekDay) timeFields

-- ** Start time

-- | Obtains the start times for each course depending on its session.
startTimesByCourse :: InfoTimeFieldsByDay -> Session -> StartTimesByDay
startTimesByCourse dataInOrder "Y" = startTime dataInOrder ++
                                     startTime dataInOrder
startTimesByCourse dataInOrder _ = startTime dataInOrder

-- | Obtains the start times for each course by day.
startTime :: InfoTimeFieldsByDay -> StartTimesByDay
startTime =
    map (\dataByDay -> map formatTimes (timesOrdered dataByDay))
    where
        timesOrdered dataDay = sort $ map startingTime dataDay

-- ** End time

-- | Obtains the end times for each course depending on its session.
endTimesByCourse :: InfoTimeFieldsByDay -> Session -> StartTimesByDay
endTimesByCourse dataInOrder "Y" = endTime dataInOrder ++
                                   endTime dataInOrder
endTimesByCourse dataInOrder _ = endTime dataInOrder

-- | Obtains the end times for each course by day.
endTime :: InfoTimeFieldsByDay -> EndTimesByDay
endTime =
    map (\dataByDay -> map formatTimes (timesOrdered dataByDay))
    where
        timesOrdered dataDay = sort $ map endingTime dataDay

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
        hour = head hours
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
    | session == "Y" = map (getDatesByDay "F") dataInOrder ++
                       map (getDatesByDay "S") dataInOrder
    | otherwise = map (getDatesByDay session) dataInOrder

-- | The string representation for a date in which an event
-- occurs for the first time.
type StartDate = String

-- | String representation of a date after which no more events
-- are created.
type EndDate = String

-- | Gives the appropiate starting and ending dates for each day,in which the
-- course takes place, depending on the course session.
getDatesByDay :: Session -> [Times'] -> (StartDate, EndDate)
getDatesByDay session dataByDay
    | session ==  "F" = formatDates $ getDatesFall $ weekDay $ head dataByDay
    | otherwise = formatDates $ getDatesWinter $ weekDay $ head dataByDay

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
