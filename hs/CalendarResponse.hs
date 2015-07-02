--Aeson, Aeson lens, Record representation 
-- Check 120. Be in any order time not the first and last Group them by day of the week and figure out consecutive blocks within that day
-- Do not look at selected courses.
-- I could use sequence at the beginning of start dates, start times, end times.
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

-- | A data type representing a list of lists of start, end times; as well as, start date for all courses.
data Triple = Triple {tripleStart :: [String], tripleEnd :: [String], tripleDates :: [[String]]} deriving (Show)

{-
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


--Start times Response
getCalendar :: String -> IO Response
getCalendar courses = fmap genRes (sequence (startTimes $ allInfo courses)) -- [IO String] -- IO [String]

genRes :: [String] -> Response
genRes start =  toResponse $ unlines start 


{-
--Names Response
getCalendar :: String -> IO Response
getCalendar courses = return $ genRes $ getNames courses

genRes :: [String] -> Response
genRes start =  toResponse $ unlines start
-}
{- Final
getCalendar :: String -> IO Response
getCalendar courses = return $ fmap (sequence $ allInfoTimes courses) -- [IO [Time]]

genRes :: [[Time]] -> Response
genRes times =  toResponse $ unlines allTimes
    where
    allFields = map (map timeField) times
    allTimes = map (map (map show)) allFields -- [[]]
-}    
{-
--End times Response
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new0 ((endTimes (allInfoTimes courses)) !! 0)
-}
{-
--Start dates Response
getCalendar :: String -> String -> IO Response
getCalendar coursesCode courses = fmap response ((startDates (allInfo courses)) !! 0)
-}
--(getNames courses)
--start = (startTimes (allInfoTimes courses))
--end = (endTimes (allInfoTimes courses))
--date = (startDates (allInfo courses))

{-response :: [String] -> Response
response dates = toResponse $ dates !! 0

new2 :: [Day] -> Response
new2 courseJSON = toResponse $ show $ courseJSON !! 0

new :: [Time] -> Response
new courseJSON = createJSONResponse $ show $ courseJSON !! 0
-}

-- | Returns a CSV file of events as requested by the user.
calendarResponse :: String -> ServerPart Response
calendarResponse courses =
    liftIO $ getCalendar courses
{-
-- Generates the Response for the server which consists of a csv file
getCalendar :: String -> IO Response
getCalendar courses = fmap response (toCSV courses)

response :: String -> Response
response events = toResponse events
-}

{- Output file:
"Subject, start date, start time, end date, end time, all day event, description, location, private
MAT137,09/14/15,8:00:00 AM,09/14/15,9:00:00 AM,False,MAT137,tba,True" 
-}
{-
-- Generate the string that represents a CSV file
toCSV :: String -> IO String
toCSV courses = do 
    allEvents <-  getAllEvents courses
    return $ unlines (["Subject,Start Date,Start Time,End Date,End Time,All Day Event,Description,Location,Private"] ++ allEvents)

getAllEvents :: String -> IO [String]
getAllEvents courses = matchData name start end date
    where
    name = (getNames courses)
    start = (startTimes (allInfo courses))
    end = (endTimes (allInfoTimes courses))
    date = (startDates (allInfo courses))

--allInfoTimes is for start and end time, and allInfo is for startDate
-- Obtain the name for all subjects
getNames :: String -> [String]
getNames courses = [code | [code, section, session] <- getCoursesInfo courses] 
-}
-- Obtain the information for all courses from the database
allInfo :: String -> [(IO [Time], (String, String))]
allInfo courses = [(pullDatabase code section session, (code, session))| [code, section, session] <- getCoursesInfo courses]

allInfoTimes :: String -> [IO [Time]]
allInfoTimes courses = map (fst) (allInfo courses)

-- Obtain a list with all the information about the courses obtained from the cookies
getCoursesInfo :: String -> [[String]]
getCoursesInfo courses = map (splitOn "-") byCourse -- [splitOn "-" course| course <- byCourse]
    where
    byCourse = splitOn "_" courses

-- Pull out the information (Time string, Time fields, code) for each course from the database
pullDatabase :: String -> String -> String -> IO [Time]
pullDatabase code section session =
    if (take 1 section) == "L" --Tried !! but did not work
    then (returnLectureTimes (T.pack code) (T.pack section) (T.pack session))
    else (returnTutorialTimes (T.pack code) (T.pack section) (T.pack session))

-- START TIME / END TIME

startTimes :: [(IO [Time], (String, String))] -> [IO String]
startTimes coursesTimes = map startTime coursesTimes 

-- Obtain the starting time for each course
startTime :: (IO [Time], (String, String)) -> IO String
startTime courseTime = fmap (getStartTime $ snd courseTime) (fst courseTime)

-- Generate the string starting time
getStartTime :: (String, String) -> [Time] -> String
getStartTime codeSession courseFields = fieldInfoTime (filtering $ (groupDays $ map timeField courseFields)) codeSession 
{-
-- Group them by day of the week and figure out consecutive blocks within that day
groupDays :: [[Double]] -> [[Double]]
groupDays courseFields = [assignDay (courseField) | courseField <- courseFields]
-}

-- Group them by day of the week and figure out consecutive blocks within that day
groupDays :: [[Double]] -> [[[Double]]]
groupDays courseFields = (joinList ([assignDay (courseField) (courseField !! 0)| courseField <- courseFields]))

filtering :: [[[Double]]] -> [[[Double]]]
filtering days = [filter p fields |fields <- days] 
    where p x = length x == 2 

assignDay :: [Double] -> Double -> [[[Double]]]
assignDay courseField day
    | day <= 0.0 = [[courseField], [[8.0]], [[8.0]], [[8.0]], [[8.0]]]
    | day == 1.0 = [[[8.0]], [courseField], [[8.0]], [[8.0]], [[8.0]]]
    | day == 2.0 = [[[8.0]], [[8.0]], [courseField], [[8.0]], [[8.0]]]
    | day == 3.0 = [[[8.0]], [[8.0]], [[8.0]], [courseField], [[8.0]]]
    | otherwise = [[[8.0]], [[8.0]], [[8.0]], [[8.0]], [courseField]] -- [[4.0,3.0],[5.0,6.0]]

joinList :: [[[[Double]]]] -> [[[Double]]]
joinList days = [monday, tuesday, wednesday, thursday, friday] 
    where
    monday = concat $ map (!! 0) days -- [ [ [[8.0]], [[8.0]], [[8.0]], [[8.0]], [[8.0]] ], [ [[8.0]], [[8.0]], [[8.0]], [[8.0]], [[8.0]] ],.. ] 
    tuesday = concat $ map (!! 1) days
    wednesday = concat $ map (!! 2) days
    thursday = concat $ map (!! 3) days
    friday = concat $ map (!! 4) days 

{-
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!" 
-}

-- Get the field that controls time
fieldInfoTime :: [[[Double]]] -> (String, String) -> String
fieldInfoTime courseFields codeSession = show (((courseFields !! 0) !! 0) !! 0)
{-
endTimes :: [IO [Time]] -> [IO String]
endTimes coursesTimes = map endTime coursesTimes 

-- Obtain the ending time for each course
endTime :: IO [Time] -> IO String
endTime courseTime = fmap getEndTime courseTime

-- Generate the string ending time
getEndTime :: [Time] -> String
getEndTime courseFields = fieldInfoEndTime $ courseFields !! position
    where
    position = (length courseFields) - 1

-- Get the field that controls time
fieldInfoEndTime :: Time -> String
fieldInfoEndTime courseField = getStr $ ((timeField courseField) !! 1) + 0.5

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
-}
{-
-- DEALING WITH DATES
-- startDates (allInfoDates courses)

startDates :: [(IO [Time], String)] -> [IO [String]]
startDates coursesDates = map startDate coursesDates 

-- Obtain the starting time for each course
startDate :: (IO [Time], String) -> IO [String]
startDate courseDate = fmap (getStartDate (snd courseDate)) (fst courseDate)

-- Generate the string starting date
getStartDate :: String -> [Time] -> [String]
getStartDate session courseFields = fieldInfoDate session (courseFields !! 0) 

-- Get the field that controls the day
fieldInfoDate :: String -> Time -> [String]
fieldInfoDate session courseField = getDate session ((timeField courseField) !! 0)

getDate :: String -> Double -> [String]
getDate session dayValue = map format (generateDate dayStr session)
    where
    dayStr = day dayValue

-- Give the appropriate day for the course given based on its position
day :: Double -> String 
day 0.0 = "M"
day 1.0 = "T"
day 2.0 = "W"
day 3.0 = "R"
day 4.0 = "F"

-- Format the date in the following way: month/day/year
format :: Day -> String
format date = formatTime defaultTimeLocale "%D" date

-- Takes data from event days to generate all the dates given the specific days
generateDate :: String -> String -> [Day]
generateDate courseDay session = if session == "F" then generateDatesFall courseDay else generateDatesWinter courseDay

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
matchData :: [String] -> [IO String] -> [IO String] -> [IO [String]] -> IO [String]
matchData names allStart allEnd allDates = matchData1 names (sequence allStart) (sequence allEnd) (sequence allDates)

matchData1 :: [String] -> IO [String] -> IO [String] -> IO [[String]] -> IO [String]
matchData1 names allStart allEnd allDates = matchData2 names (sequenceMe (allStart, allEnd, allDates))

matchData2 :: [String] -> IO ([String], [String], [[String]]) -> IO [String]
matchData2 names allStartEndDates = fmap (matchInfo names) allStartEndDates

matchInfo :: [String] -> ([String], [String], [[String]]) -> [String]
matchInfo names allStartEndDates = concat [eventsByCourse (names !! i) ((ft1 allStartEndDates) !! i) ((sd1 allStartEndDates) !! i) ((thr1 allStartEndDates) !! i)|  i <- [0 .. x]]
    where
    x = (length $ ft1 allStartEndDates) - 1

-- Generate the string that represents the event for each course
eventsByCourse :: String -> String -> String -> [String] -> [String]
eventsByCourse name start end date =  [name ++ "," ++ byDate ++ "," ++ start ++ "," ++ byDate ++ "," ++ end ++ ",False," ++ name ++ ",tba,True"| byDate <- date] 

sequenceMe :: (IO [String], IO [String], IO [[String]]) -> IO ([String], [String], [[String]])
sequenceMe allStartEndDates = do
    start <- ft allStartEndDates
    end <- sd allStartEndDates
    dates <- thr allStartEndDates
    return (start, end, dates)


ft:: (IO [String], IO [String], IO [[String]]) -> IO [String]
ft (x,_,_) = x

ft1:: ([String], [String], [[String]]) -> [String]
ft1 (x,_,_) = x

sd :: (IO [String], IO [String], IO [[String]]) -> IO [String]
sd (_,x,_) = x

sd1 :: ([String], [String], [[String]]) -> [String]
sd1 (_,x,_) = x

thr :: (IO [String], IO [String], IO [[String]]) -> IO [[String]]
thr (_,_,x) = x

thr1 :: ([String], [String], [[String]]) -> [[String]]
thr1 (_,_,x) = x
-}
{-
matchData :: [String] -> [IO String] -> [IO String] -> [IO [String]] -> IO [String]
matchData names allStart allEnd allDates = matchInfo names (sequence allStart) (sequence allEnd) (sequence allDates)

matchData1 :: [String] -> IO [String] -> IO [String] -> IO [[String]] -> IO [String]
matchData1 names allStart allEnd allDates = concat [matchData2 names (sequence [allStart, allEnd, fmap (!! i) allDates]) | i <- [0 .. x]]
    where
    x = (length allStart) - 1

matchData2 :: [String] -> IO [[String], [String], [String]] -> IO [String]
matchData2 names allDates allStartEnd = fmap (matchInfo (names, allDates)) allStartEndDates

matchInfo :: ([String]) -> [[String], [String], [[String]]] -> [String]
matchInfo names allStartEndDates = concat [eventsByCourse (names !! i) ((allStartEndDates !! 0) !! i) ((allStartEndDates !! 1) !! i) ((allStartEndDates !! 2) !! i)|  i <- [0 .. x]]
    where
    x = (length $ allStartEndDates !! 0) - 1

-- Generate the string that represents the event for each course
eventsByCourse :: String -> String -> String -> [String] -> [String]
eventsByCourse name start end date =  [name ++ "," ++ date ++ "," ++ start ++ "," ++ date ++ "," ++ end ++ ",False," ++ name ++ ",tba,True"| byDate <- date] 

-}



{-
matchData1 :: IO [String] -> IO [String] -> IO [[String]] -> IO [String]
matchData1 allStart allEnd allDates = fmap (matchData2 (allStart,allEnd)) allDates

matchData2 :: (IO [String], IO [String]) -> [[String]] -> IO [String]
matchData2 allStartEnd allDates = fmap (matchData3 ((fst allStartEnd), allDates)) (snd allStartEnd))

matchData3 :: (IO [String], [[String]]) -> [String] -> IO [String]
matchData3 allStartDates allEnd = fmap (matchInfo ((snd allStartEnd), allEnd)) (fst allStartEnd))

matchInfo :: ([String], [[String]]) -> [String] -> [String]
matchInfo allEndDates allStart = [createEvent ((allStart !! i) ((fst allEndDates) !! i) ((snd allEndDates) !! i))| i <- [0 .. x]]
    where
    x = (length allStart) - 1
-}




















{-
allInfoTimes :: String -> [IO [Double]] -- This string has the times as double. I need to make them look like GCalendar time
allInfoTimes courses = [fmap (map (!! 1)) timeField | timeField <- timeFields]
    where
    timeFields = fieldsInfoTimes courses -- [IO [ [no,need],[no,need],[], .. ]]

 -- Starting time
startTimes1 :: String -> [IO String]
startTimes1 courses = map (fmap getStr) startField -- ** to fill with from double to str -- Make it a string and use splitOn "."
    where
    startField = startTime1 courses 

-- Getting the first time field which contains the starting time
startTime1 :: String -> [IO Double]
startTime1 courses = map (fmap (!! 0)) timesInfo
    where
    timesInfo = allInfoTimes courses


endTimes1 :: String -> [IO String]
endTimes1 courses = map (fmap getStr) endField
    where
    endField = endTime1 courses

-- Getting the last time field which contains the ending time
endTime1 :: String -> [IO Double]
endTime1 courses = map (fmap(!! position)) timesInfo
    where 
    timesInfo = allInfoTimes courses
    position = (length timesInfo) - 1 

allInfoDates :: String -> [IO T.Text]
allInfoDates courses = map (fmap fst) (allInfo courses) 

-}

{-
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

-- The ending time for each subject --Be careful and add 0.5 to the last time field
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
-}

{-
-- Response for endTimes Prev
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = fmap new0 ((endTimes lectures) !! 0)
-}

{-
-- Response for the dates
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap response ((startDate courses) !! 0)

-}


{-
-- Response for new startTime
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = fmap new0 ((startTimes1 lectures) !! 0)
-}

{-
--Last try to get the times from the Time fields
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new4 ((allInfoTimes courses) !! position)
    where
    position = (length $ allInfoTimes courses) - 1

new4 :: [Double] -> Response
new4 course = toResponse $ show $ course !! 1
-}

{-
-- Contructor type that I did not know
getCalendar :: String -> String -> IO Response -- startDate :: String -> String -> [IO [Day]]
getCalendar coursesCode courses = fmap new4 ((allInfoTimes courses) !! 0)

new4 :: [Time] -> Response
new4 course = toResponse $ show $ (timeField $ course !! 0) !! 1
-}

{-
-- Response for startTimes Prev
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = fmap new0 ((startTimes lectures) !! 0)
-}


{-
-- Response for new endTime
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = fmap new0 ((endTimes1 lectures) !! 0)
-}

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
getCalendar courses lectures = fmap new ((infoTimes lectures) !! position)
    where
    position = (length $ infoTimes courses) - 1
-}

-- toResponse (startDate (allInfo (getCoursesInfo lectures)))
--liftIO $ print (startDate (allInfo (getCoursesInfo lectures)))




-- PROGRAM BEFORE
-- PROGRAM BEFORE
-- PROGRAM BEFORE

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

