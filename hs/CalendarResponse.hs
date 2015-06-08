module CalendarResponse where

import Data.List.Split (splitOn)
import Data.List
import Data.List
import Data.Time
import Happstack.Server
import Control.Monad.IO.Class (liftIO)
import System.Locale
import Text.Blaze ((!))
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Scripts

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
startDate :: [[String]] -> [[Day]]
startDate courses = [generateDates days | days <- eventDays courses]

-- Generate all the dates given the specific days
-- First day of classes will be on September, September 14.
generateDates :: String -> [Day]
generateDates "M" = take 30 [addDays i firstMonday | i <- [0,7..]]
    where 
    firstMonday = fromGregorian 2015 09 14
generateDates "T" = take 30 [addDays i firstTuesday | i <- [0,7..]]
    where 
    firstTuesday = fromGregorian 2015 09 15
generateDates "W" = take 30 [addDays i firstWednesday | i <- [0,7..]]
    where 
    firstWednesday = fromGregorian 2015 09 16
generateDates "R" = take 30 [addDays i firstThursday | i <- [0,7..]]
    where 
    firstThursday = fromGregorian 2015 09 17
generateDates "F" = take 30 [addDays i firstFriday | i <- [0,7..]]
    where 
    firstFriday = fromGregorian 2015 09 18

-- Same as startDate, since our events do not happen in more than one day
endDate :: [[String]] -> [[Day]]
endDate courses = startDate courses


{- Output file:
"Subject, start date, start time, end date, end time, all day event, description, location, private
MAT137,09/14/15,8:00:00 AM,09/14/15,9:00:00 AM,False,MAT137,tba,True" 
-}
getCsvFile :: String -> String -> String
getCsvFile courses session = toCSV(matchData (startTimes coursesWeekly) (endTimes coursesWeekly) (startDate coursesWeekly))
    where
    coursesWeekly = splitCourses courses session

splitCourses :: String -> String -> [[String]]
splitCourses courses session = partition5 $ splitOn "_" courses
    where
    partition5 [] = []
    partition5 lst = take 5 lst : partition5(drop 5 lst)

-- Generate the string that represents a CSV file
toCSV :: [String] -> String
toCSV eventsData = unlines ([title] ++ eventsData)
    where
    title = "Subject,Start Date,Start Time,End Date,End Time,All Day Event,Description,Location,Private"

-- Put together all the information in the corresponding order given by the lists
matchData :: [(String,String)] -> [(String,String)] -> [[Day]] -> [String]
matchData start end date = concat [eventsByCourse (start !! i) (end !! i) (date !! i) | i <- [0..x]]
    where
    x = (length start) - 1

-- Generate the string that represents the event for each course
eventsByCourse :: (String,String) -> (String,String) -> [Day] -> [String]
eventsByCourse start end date =  [fst start ++ "," ++ format byDate ++ "," ++ snd start ++ "," ++ format byDate ++ "," ++ snd end ++ ",False," ++ fst end ++ ",tba,True"| byDate <- date] 

-- | Returns a CSV file of events as requested by the user.
calendarResponse :: String -> String -> ServerPart Response
calendarResponse courses session =
    liftIO $ getCalendar courses session

-- Generates a response, which is a CSV file
getCalendar :: String -> String -> IO Response
getCalendar courses session = return $ toResponse(getCsvFile courses session)