module CalendarResponse where

import Data.List.Split (splitOn)
import Data.List
import Data.Time
import Happstack.Server
--import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Locale
import Config (firstMondayFall, firstMondayWinter)
import Database.CourseQueries (returnCourse)-- For  returnCourse
import Data.Text (pack) -- For unpack
import Database.Tables as Tables --  For the IO Course response issue
import JsonResponse

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

-- Generate all the dates given the specific days
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

-- courses: MAT137Y1   lectures: MAT137Y1-L5101-Y
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = do
    courseJSON <- returnCourse (pack "MAT137Y1-L5101-Y")
    return $ toResponse (createJSONResponse courseJSON)
-- getCalendar courses lectures = return $ toResponse(returnCourse (pack "MAT137Y1"))

{-
getCalendar :: String -> String -> IO Response
getCalendar courses lectures = return $ toResponse ("courses: " ++ courses ++ " Lectures: " ++ lectures)
-}
calendarResponse :: String -> String -> ServerPart Response
calendarResponse courses lectures =
    liftIO $ getCalendar courses lectures

{-getInfoDatabase :: String -> String -> String 
getInfoDatabase courses lectures =  $ do
    basic <- selectList [Lecturescode ==. "MAT137Y1-L5101-Y"] [] --Second list is an ouput option
    returnCourse (unpack "MAT137Y1-L5101-Y")
    liftIO $ print basic



getInfoDatabase :: String -> String -> IO Course
getInfoDatabase courses lectures =  returnCourse (unpack "MAT137Y1-L5101-Y")
-}