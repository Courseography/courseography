--Aeson, Aeson lens, Record representation 
module CalendarResponse where

import Data.List.Split (splitOn)
import Data.List
import Data.Time
import Happstack.Server
import Control.Monad.IO.Class (liftIO)
--import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Locale
import Config (firstMondayFall, firstMondayWinter)
import Database.CourseQueries (returnCourse, returnTutorialTimes, returnLectureTimes)-- For  returnCourse
import qualified Data.Text as T
import Database.Tables as Tables --  For the IO Course response issue
import JsonResponse
import Data.Aeson (encode, decode)
-- import Text.JSON
import Database.Persist

lecturesStr = "CHM138H1-P0101-F_MAT136H1-T0201-S"
coursesStr = ""
{-
-- EVENTS' NAME, START/END TIME

-- List of the subjects
names :: [[String]] -> [String]
names courses = [subject | subject <- concat courses, not(null subject)]

-- The start time for each subject
startTimes :: [[String]] -> [(String,String)]
startTimes courses = concat(matchingTime(zip (courses) ([show x ++ ":00:00 AM"| x <- [8..11]] ++ [show x ++ ":00:00 PM"| x <- ([12] ++ [1..9])])))

-- The ending time for each subject
endTimes :: [[String]] -> [(String,String)]
endTimes courses = concat(matchingTime(zip (courses) ([show x ++ ":00:00 AM"| x <- [9..11]] ++ [show x ++ ":00:00 PM"| x <- ([12] ++ [1..10])])))

-- Match the time with the corresponding subject
matchingTime :: [([String],String)] -> [[(String,String)]]
matchingTime timeCourses = justValid [zip (fst timeCourse) [snd timeCourse | x <- [0..7]] | timeCourse <- timeCourses]

-- Just take the valid tuples that have actual subjects instead of empty strings
justValid :: [[(String,String)]] -> [[(String, String)]]
justValid timeCoursesList = let notNull x = not (null x) in filter notNull [[timeCourse | timeCourse <- timeCourses, not(null(fst(timeCourse)))] | timeCourses <- timeCoursesList]


-- EVENTS' START/END DATE

-- Format the date in the following way: month/day/year
format :: Day -> String
format date = formatTime defaultTimeLocale "%D" date

-- Create a list with all the days in which courses take place
eventDays :: [[String]] -> [String] 
eventDays courses = allDays [giveDay byWeek| byWeek <- courses]

-- Create a list with all the days
allDays :: [[String]] -> [String] 
allDays listDays = [justDays | justDays <- concat listDays, not(null justDays)]

-- Get the day in which each course takes place
giveDay :: [String] -> [String]
giveDay coursesWeek = [if null(coursesWeek !! i) then "" else day i  | i <- [0,1,2,3,4]]

-- Give the appropriate day for the course given based on its position
day :: Int -> String 
day 0 = "M"
day 1 = "T"
day 2 = "W"
day 3 = "R"
day 4 = "F"

-- Takes data from event days to generate all the dates given the specific days
startDate :: [[String]] -> String -> [[Day]]
startDate courses session = [if session == "Fall" then generateDatesFall days else generateDatesWinter days | days <- eventDays courses]
-}

{-
-- Same as startDate, since our events do not happen in more than one day
endDate :: [[String]] -> String -> [[Day]]
endDate courses session = startDate courses session

{- Output file:
"Subject, start date, start time, end date, end time, all day event, description, location, private
MAT137,09/14/15,8:00:00 AM,09/14/15,9:00:00 AM,False,MAT137,tba,True" 
-}
-- Generate the string that represents a CSV file
toCSV :: String -> String -> String
toCSV coursesFall coursesWinter = unlines ([title] ++ getAllevents coursesFall "Fall" ++ getAllevents coursesWinter "Winter")
    where
    title = "Subject,Start Date,Start Time,End Date,End Time,All Day Event,Description,Location,Private"

-- Generate all the events based on the string courses given
getAllevents :: String -> String ->[String]
getAllevents courses session= (matchData (startTimes coursesWeekly) (endTimes coursesWeekly) (startDate coursesWeekly session))
    where
    coursesWeekly = splitCourses courses

-- Divide the string courses into weekly portions
splitCourses :: String -> [[String]]
splitCourses courses = partition5 $ splitOn "_" courses
    where
    partition5 [] = []
    partition5 lst = take 5 lst : partition5(drop 5 lst)

-- Put together all the information in the corresponding order given by start, end and date
matchData :: [(String,String)] -> [(String,String)] -> [[Day]] -> [String]
matchData start end date = concat [eventsByCourse (start !! i) (end !! i) (date !! i) | i <- [0..x]]
    where
    x = (length start) - 1

-- Generate the string that represents the event for each course
eventsByCourse :: (String,String) -> (String,String) -> [Day] -> [String]
eventsByCourse start end date =  [fst start ++ "," ++ format byDate ++ "," ++ snd start ++ "," ++ format byDate ++ "," ++ snd end ++ ",False," ++ fst end ++ ",tba,True"| byDate <- date] 

-- | Returns a CSV file of events as requested by the user.
{-calendarResponse :: String -> String -> ServerPart Response
calendarResponse coursesFall coursesWinter =
    liftIO $ getCalendar coursesFall coursesWinter

-- Generates a response, which is a CSV file
getCalendar :: String -> String -> IO Response
getCalendar coursesFall coursesWinter = return $ toResponse(toCSV coursesFall coursesWinter)

calendarResponse :: String -> String -> String -> ServerPart Response
calendarResponse coursesFall coursesWinter cookie =
    liftIO $ getCalendar coursesFall coursesWinter cookie

getCalendar :: String -> String -> String -> IO Response
getCalendar coursesFall coursesWinter cookie = return $ toResponse(cookie)



calendarResponse :: String -> String -> ServerPart Response
calendarResponse courses lectures =
    liftIO $ getCalendar courses lectures
-}
-}


{-}
-- 2222222222222222222222222222222222222222222222
-- ________________________________________MAT135 (T)______________________________
-- Using return course
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = do
    courseJSON <- (returnTutorialTimes (T.pack "MAT135H1") (T.pack "F") (T.pack "T0501"))
    return $ toResponse ((show (ft courseJSON)) ++ (show (sd courseJSON)) ++ (show (thr courseJSON)))

ft:: (T.Text, [Time], T.Text) -> T.Text
ft (x,_,_) = x

sd :: (T.Text, [Time], T.Text) -> [Time]
sd (_,x,_) = x

thr :: (T.Text, [Time], T.Text) -> T.Text
thr (_,_,x) = x
-- courseJSON <- returnCourse (pack "MAT137Y1-L5101-Y")
-- getCalendar courses lectures = return $ toResponse(returnCourse (pack "MAT137Y1"))
-}
{-
-- Using returnTutorials returning just timeStr
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = do
    courseJSON <- (returnTutorialTimes (T.pack "MAT135H1") (T.pack "T0501") (T.pack "F"))
    return $ toResponse (show courseJSON)
-}

{-
-- 33333 
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = do
    courseJSON <- (returnCourse (T.pack "mat135h1"))
    return $ toResponse (createJSONResponse courseJSON)
-}

{-
--Transfrorm to lower case every course given
allCourses :: String -> [IO Course]
allCourses courses = [returnCourse course | course <- toLowerCourse courses]
-}

-- Obtain the information for all courses from the database
allInfo :: String -> [IO (T.Text, [Time])]
allInfo courses = [pullDatabase code section session| [code, section, session] <- getCoursesInfo courses]

allInfoTimes :: String -> [IO [Time]]
allInfoTimes courses = map (fmap snd) (allInfo courses)

allInfoDates :: String -> [IO T.Text]
allInfoDates courses = map (fmap fst) (allInfo courses) 

-- Pull out the information (Time string, Time fields, code) for each course from the database
pullDatabase :: String -> String -> String -> IO (T.Text, [Time])
pullDatabase code section session =
    if (take 1 section) == "L" --Tried !! but did not work
    then (returnLectureTimes (T.pack code) (T.pack section) (T.pack session))
    else (returnTutorialTimes (T.pack code) (T.pack section) (T.pack session))

-- Obtain a list with all the information about the courses obtained from the cookies
getCoursesInfo :: String -> [[String]]
getCoursesInfo lectures = map (splitOn "-") byCourse -- [splitOn "-" course| course <- byCourse]
    where
    byCourse = splitOn "_" lectures

-- start dates in the right format month/day/year
startDate :: String -> [IO [String]]
startDate courses = [fmap (map format) bydate  | bydate <- date courses] 

--  start dates in the format year-month-day
date :: String -> [IO [Day]]
date courses = [generateDate (coursesInfo !! i) ((sessions !! i) !! 2) | i <- [0 .. l]]
    where
    coursesInfo = allInfoDates courses -- [IO T.Text]
    sessions = getCoursesInfo courses -- [[String]]
    l = (length coursesInfo) - 1

-- Takes data from event days to generate all the dates given the specific days
generateDate :: IO T.Text -> String -> IO [Day]
generateDate course session = if session == "F" then fmap generateDatesFall day else fmap generateDatesWinter day
    where
    day = eventDays course

-- Days in which courses take place
eventDays :: IO T.Text -> IO String 
eventDays course = fmap getDay course

getDay :: T.Text -> String
getDay course = toStr $ T.head course

toStr :: Char -> String
toStr item = [item] 

-- Format the date in the following way: month/day/year
format :: Day -> String
format date = formatTime defaultTimeLocale "%D" date

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

-- START TIME

-- The start time for each subject "F3" "M2-5" "T1:30-4:30" IO Text
startTimes :: String -> [IO String]
startTimes courses = map (fmap startTime) infoDatesTimes
    where
    infoDatesTimes = allInfo courses

-- The ending time for each subject
endTimes :: String -> [IO String]
endTimes courses = map (fmap endTime) infoDatesTimes
    where
    infoDatesTimes = allInfo courses

startTime :: (T.Text, [Time]) -> String
startTime times = fst $ getTime (fst times) (snd times)

endTime :: (T.Text, [Time])  -> String
endTime times = snd $ getTime (fst times) (snd times)

getTime :: T.Text -> [Time] -> (String, String)
getTime courseText times = if (T.length $ T.pack course) == 1 then (course, endTimeHour) else (startTime, endTimeHours) 
    where
    course = T.unpack $ T.tail courseText
    endTimeHour = show (read course + 1)
    times = splitOn "-" course
    startTime = times !! 0
    endTimeHours = times !! 1

new0 :: String -> Response
new0 day = toResponse day

new2 :: [Day] -> Response
new2 courseJSON = toResponse $ show $ courseJSON !! 0

new :: [Time] -> Response
new courseJSON = createJSONResponse $ show $ courseJSON !! 0

response :: [String] -> Response
response dates = toResponse $ dates !! 0 


-- Contructor type that I do not know
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new4 ((allInfoTimes courses) !! 0)

new4 :: [Time] -> Response
new4 course = toResponse $ show $ timeTimeField $ course !! 0


{-
-- Response for startTimes
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = fmap new0 ((startTimes lectures) !! 0)
-}

{-
-- Response for endTimes
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = fmap new0 ((endTimes lectures) !! 0)
-}

{-
-- Response for the dates
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap response ((startDate courses) !! 0)
-}

calendarResponse :: String -> String -> ServerPart Response
calendarResponse coursesCode courses =
    liftIO $ getCalendar coursesCode courses

{-
-- courses: MAT137Y1   lectures: MAT137Y1-L5101-Y        MAT135H1 MAT135H1-L0101-F 
-- Just getting the response
-- 111111111111111111111111111111111111111
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = return $ toResponse (lectures)
-}
{-
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new2 ((startDate courses) !! 0)
-}

{-
foo :: Time
foo = time timeField = [0.0,14.0]
-}
-- Call returnCourse on that Data
-- Look for the lecture/tutorial and tutorial/lecture time

{-
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = do
    lecture <- selectList [LecturesCode ==. (pack "MAT135H1")] [] -- I am getting a lecture here
    return $ toResponse (course)
--lecture <- selectList [LecturesCode ==. (pack "MAT135H1"), LecturesTimes !=. []] []
-}

{-
-- maybePerson <- getBy $ code "MAT135H1-L0101-F"
getCalendar :: String -> String -> IO Response
getCalendar courses lecture = do
    maybeLectures <- getBy $ LecturesCode (pack "MAT135H1-L0101-F")
    case maybeLectures of
        Nothing -> return $ toResponse "There is no such a course"
        Just (Entity (LecturesCode "MAT135H1-L0101-F") lectures) -> return $ toResponse lectures
-}

-- Just (Entity LecturesCode lectures) -> return $ toResponse lectures

{-
-- Doing just selectList
getCalendar:: String -> String -> IO Response
getCalendar courses lectures = do
    course <- selectList [CoursesCode ==. (pack "MAT135H1")] []
    liftIO $ print course
    return $ toResponse course
    course <- getBy $ CourseCodeKey "MAT135H1"
    case course of
        Nothing -> liftIO $ print "This course is not in the database."
        Just row -> do
            lecture <- selectList [LectureCode ==. entityKey row] []
            liftIO $ print tuts
-}

{-getInfoDatabase :: String -> String -> String 
getInfoDatabase courses lectures =  $ do
    basic <- selectList [Lecturescode ==. "MAT137Y1-L5101-Y"] [] --Second list is an ouput option
    returnCourse (unpack "MAT137Y1-L5101-Y")
    liftIO $ print basic

getInfoDatabase :: String -> String -> IO Course
getInfoDatabase courses lectures =  returnCourse (unpack "MAT137Y1-L5101-Y")
-}

-- To do: merge all d}ata, check google format for hours like 2:30
{-
--lengths
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = return $ toResponse ("len1: " ++ len1 ++ "  %  len2: " ++ len2)
    where
    len1 = show $ length $ allInfoDates courses
    len2 = show $ length $ getCoursesInfo courses

new7 :: String -> Response
new7 course = toResponse course
-}

{-
-- Getting a day code
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new6 (eventDays ((allInfoDates courses) !! 0))

new6 :: String -> Response
new6 course = toResponse course
-}

{- 
-- Not working non-exhaustive pattern in generateDatesFall
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new5 (date ((allInfoDates courses) !! 0) "Fall")

new5 :: [Day] -> Response
new5 course = toResponse $ show $ course !! 0
-}

{-
-- allInfoDates testing
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new3 ((allInfoDates courses) !! 0)

new3 :: T.Text -> Response
new3 course = toResponse $ show course
-} 

-- MatchData
-- starTimes, endTimes [IO String]; startDate [IO [Day]]


{-
-- Fails becuase of generateDatesFall
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new2 (fmap generateDatesFall (eventDays $ (allInfoDates courses) !! 0))
-}
{-
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = do
    [fmap new8 day | day <- [eventDays code| code <- allInfoDates courses]]
    return $ toResponse accumstr

accumstr = ""

new8 :: String -> String
new8 dayStr = accumstr ++ dayStr
 -}

{-
-- Getting just the code
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new0 (eventDays $ (allInfoDates courses) !! 0)
-}

{-
-- Generating the non-exhaustive error
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = return $ new2 (generateDatesFall "m")
-}

{-
-- Final getCalendar
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = fmap new ((allInfoTimes lectures) !! 0)
-}

-- toResponse (startDate (allInfo (getCoursesInfo lectures)))
--liftIO $ print (startDate (allInfo (getCoursesInfo lectures)))
