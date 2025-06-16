module Controllers.Timetable
    (gridResponse, returnPDF, exportTimetableImageResponse,
     exportTimetablePDFResponse, calendarResponse) where

import Config (fallEndDate, fallStartDate, holidays, outDay, runDb, winterEndDate, winterStartDate)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Base64.Lazy as BEnc
import qualified Data.ByteString.Lazy as L
import Data.List (groupBy, sort, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, formatTime, getCurrentTime, toGregorian)
import Data.Time.Calendar.OrdinalDate (fromMondayStartWeek, mondayStartWeek)
import Database.CourseQueries (returnMeeting)
import Database.Persist.Sqlite (entityKey, entityVal, selectList, (==.))
import Database.Tables
import Export.GetImages
import Export.LatexGenerator
import Export.PdfGenerator
import Happstack.Server
import MasterTemplate
import Response.Image (returnImageData)
import Scripts
import System.Directory (removeFile)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)

gridResponse :: ServerPart Response
gridResponse =
    ok $ toResponse $
        masterTemplate "Courseography - Grid"
            []
            (do
                H.div ! A.id "navbar" $ ""
                H.div ! A.id "grid-body"! A.class_ "row main" $ ""
            )
            timetableScripts


-- | Returns an image of the timetable requested by the user.
exportTimetableImageResponse :: ServerPart Response
exportTimetableImageResponse = do
    session <- lookText' "session"
    selectedCourses <- lookText' "courses"
    (svgFilename, imageFilename) <- liftIO $ getActiveTimetable selectedCourses session
    liftIO $ returnImageData svgFilename imageFilename

-- | Returns a PDF containing graph and timetable requested by the user.
exportTimetablePDFResponse :: ServerPart Response
exportTimetablePDFResponse = do
    selectedCourses <- lookText' "courses"
    graphInfo <- look "JsonLocalStorageObj"
    (graphSvg, graphImg) <- liftIO $ getActiveGraphImage graphInfo
    (fallsvgFilename, fallimageFilename) <- liftIO $ getActiveTimetable selectedCourses "Fall"
    (springsvgFilename, springimageFilename) <- liftIO $ getActiveTimetable selectedCourses "Spring"
    pdfName <- liftIO $ returnPDF graphSvg graphImg fallsvgFilename fallimageFilename springsvgFilename springimageFilename
    liftIO $ returnPdfBS pdfName

-- | Returns 64base bytestring of PDF for given name, then deletes PDF from local.
returnPdfBS :: String -> IO Response
returnPdfBS pdfFilename = do
    pdfData <- BS.readFile pdfFilename
    _ <- removeFile pdfFilename
    return $ toResponseBS "application/pdf" $ BEnc.encode $ L.fromStrict pdfData

-- | Returns the name of a generated pdf that contains graphImg and timetableImg
-- and deletes all of the img and svg files passed as arguments
returnPDF :: String -> String -> String -> String -> String -> String -> IO String
returnPDF graphSvg graphImg fallTimetableSvg fallTimetableImg springTimetableSvg springTimetableImg = do
    rand <- randomName
    let texName = rand ++ ".tex"
        pdfName = rand ++ ".pdf"
    generateTex [graphImg, fallTimetableImg, springTimetableImg] texName -- generate a temporary TEX file
    createPDF texName                            -- create PDF using TEX and delete the TEX file afterwards
    mapM_ removeFile [graphSvg, graphImg, fallTimetableSvg, fallTimetableImg, springTimetableSvg, springTimetableImg]
    return pdfName


-- | Returns an ICS file of events as requested by the user.
calendarResponse :: ServerPart Response
calendarResponse = do
    courses <- lookText' "courses"
    liftIO $ getCalendar courses

-- | Gets together all the pieces of the program.
getCalendar :: T.Text -> IO Response
getCalendar courses = do
    let courseInfo = getCoursesInfo courses
    databaseInfo <- mapM pullDatabase courseInfo
    currentTime <- getCurrentTime
    let systemTime = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" currentTime
    events <- mapM (getEvents systemTime) databaseInfo
    let icsContent = getICS (concat events)
    return $ toResponse icsContent

-- | A list of all the events created for a course.
type Events = [String]

-- | A string representaion for an ICS file.
type ICSFile = String

-- | Generates a string representing an ICS file.
getICS :: Events -> ICSFile
getICS [] = ""
getICS events = unlines $ header_ ++ events ++ bottom
    where
        header_ = ["BEGIN:VCALENDAR",
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
pullDatabase :: (Code, Section, Session) -> IO MeetTime'
pullDatabase (code, section, session) = runDb $ do
    meet <- returnMeeting code fullSection session
    allTimes <- selectList [TimesMeeting ==. entityKey meet] []
    parsedTime <- mapM (buildTime . entityVal) allTimes
    return $ MeetTime' (entityVal meet) parsedTime
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
getEvents :: SystemTime -> MeetTime' -> IO [String]
getEvents systemTime lect = do
    courseInfo <- getCourseInfo lect  -- Get the course information
    let startTimes = third courseInfo   -- Extract start times
        endTimes = fourth courseInfo     -- Extract end times
        dates = fifth courseInfo         -- Extract dates
    events <- mapM (eventsByDate courseInfo) (zip' startTimes endTimes dates)
    return (concat events)
    where
        eventsByDate :: (Code, Section, StartTimesByDay, EndTimesByDay, DatesByDay) -> ([String], [String], (String, String)) -> IO [String]
        eventsByDate courseInfo (start, end, dates) = do
            eventLists <- mapM (formatEvents courseInfo dates) (zip start end)
            return (concat eventLists)

        formatEvents :: (Code, Section, StartTimesByDay, EndTimesByDay, DatesByDay) -> (String, String) -> (String, String) -> IO [String]
        formatEvents courseInfo (startDate, endDate) (start1, end1)
            | startDate == "" || endDate == "" = return []
            | otherwise = do
                holidayList <- holidays
                let eventLines =
                        ["BEGIN:VEVENT",
                         "DTSTAMP:" ++ systemTime,
                         "DTSTART;TZID=America/Toronto:" ++ startDate ++ start1,
                         "DTEND;TZID=America/Toronto:" ++ startDate ++ end1,
                         "RRULE:FREQ=WEEKLY;UNTIL=" ++ endDate ++ "000000Z"]
                        ++ map (\date -> "EXDATE;TZID=America/Toronto:" ++ date ++ start1) holidayList
                        ++ ["ORGANIZER:University of Toronto",
                            "SUMMARY:" ++ T.unpack (first courseInfo) ++ " " ++ T.unpack (second courseInfo),
                            "CATEGORIES:EDUCATION",
                            "END:VEVENT"]
                return eventLines

-- | A list including all the start times for a course ordered by day.
type StartTimesByDay = [[String]]

-- | A list including all the end times for a course ordered by day.
type EndTimesByDay = [[String]]

-- | A list containing all the dates for a course ordered by day.
type DatesByDay = [(StartDate, EndDate)]

-- | Obtains all the necessary information to create events for a course,
-- such as code, section, start times, end times and dates.
getCourseInfo :: MeetTime' -> IO (Code, Section, StartTimesByDay, EndTimesByDay, DatesByDay)
getCourseInfo meeting = do
    let meet = meetData meeting
        allTimes = timeData meeting
        code = meetingCode meet
        sect = meetingSection meet
        dataInOrder = orderTimeFields allTimes
        start = startTimesByCourse dataInOrder (meetingSession meet)
        end = endTimesByCourse dataInOrder (meetingSession meet)
    dates <- getDatesByCourse dataInOrder (meetingSession meet)
    return (code, sect, start, end, dates)

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
type InfoTimeFieldsByDay = [[Time]]

-- | Orders by day the start and endtimes obtained from the database.
orderTimeFields :: [Time] -> InfoTimeFieldsByDay
orderTimeFields timeFields = groupBy (\x y -> weekDay x == weekDay y) sortedList
    where
        sortedList = sortOn weekDay timeFields

-- ** Start time

-- | Obtains the start times for each course depending on its session.
startTimesByCourse :: InfoTimeFieldsByDay -> Session -> StartTimesByDay
startTimesByCourse dataInOrder "Y" = startTime dataInOrder ++
                                     startTime dataInOrder
startTimesByCourse dataInOrder _ = startTime dataInOrder

-- | Obtains the start times for each course by day.
startTime :: InfoTimeFieldsByDay -> StartTimesByDay
startTime =
    map (map formatTimes . timesOrdered)
    where
        timesOrdered dataDay = sort $ map startHour dataDay

-- ** End time

-- | Obtains the end times for each course depending on its session.
endTimesByCourse :: InfoTimeFieldsByDay -> Session -> StartTimesByDay
endTimesByCourse dataInOrder "Y" = endTime dataInOrder ++
                                   endTime dataInOrder
endTimesByCourse dataInOrder _ = endTime dataInOrder

-- | Obtains the end times for each course by day.
endTime :: InfoTimeFieldsByDay -> EndTimesByDay
endTime =
    map (map formatTimes . timesOrdered)
    where
        timesOrdered dataDay = sort $ map endHour dataDay

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
getDatesByCourse :: InfoTimeFieldsByDay -> Session -> IO DatesByDay
getDatesByCourse dataInOrder session
    | session == "Y" = do
        fallDates <- mapM (getDatesByDay "F") dataInOrder
        winterDates <- mapM (getDatesByDay "S") dataInOrder
        return (fallDates ++ winterDates)
    | otherwise = mapM (getDatesByDay session) dataInOrder

-- | The string representation for a date in which an event
-- occurs for the first time.
type StartDate = String

-- | String representation of a date after which no more events
-- are created.
type EndDate = String

-- | Gives the appropriate starting and ending dates for each day, in which the
-- course takes place, depending on the course session.
getDatesByDay :: Session -> [Time] -> IO (StartDate, EndDate)
getDatesByDay session dataByDay
    | session == "F" = do
        fallStart <- fallStartDate
        fallEnd <- fallEndDate
        formatDates $ getDates fallStart fallEnd (weekDay $ head dataByDay)
    | otherwise = do
        winterStart <- winterStartDate
        winterEnd <- winterEndDate
        formatDates $ getDates winterStart winterEnd (weekDay $ head dataByDay)

-- | Formats the date in the following way: YearMonthDayT.
-- For instance, 20150720T corresponds to July 20th, 2015.
formatDates :: (Day, Day) -> IO (StartDate, EndDate)
formatDates (start, end) = do
    outDate <- outDay
    if start == outDate || end == outDate
        then return ("", "")
        else return (startStr, endStr)
    where
        startStr = formatTime defaultTimeLocale "%Y%m%dT" start
        endStr = formatTime defaultTimeLocale "%Y%m%dT" end

-- | Gives the appropriate starting and ending dates for courses in the given
-- range.
getDates :: Day -> Day -> Double -> (Day, Day)
getDates startDate endDate x =
    (fromMondayStartWeek year firstWeek' dayOfWeek,
     fromMondayStartWeek year endWeek' dayOfWeek)
    where
        (year, _, _) = toGregorian startDate
        (firstWeek, firstDay) = mondayStartWeek startDate
        (endWeek, endDay) = mondayStartWeek endDate

        convertDay 0.0 = 1
        convertDay 1.0 = 2
        convertDay 2.0 = 3
        convertDay 3.0 = 4
        convertDay _ = 5

        dayOfWeek = convertDay x

        firstWeek' = if firstDay <= dayOfWeek then firstWeek else firstWeek + 1
        endWeek' = if endDay >= dayOfWeek then endWeek else endWeek - 1
