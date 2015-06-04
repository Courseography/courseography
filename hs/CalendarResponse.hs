module CalendarResponse where

import Data.List.Split (splitOn)
import Data.List
import Data.List
import Data.Time
--import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, Response, ServerPart)
--import Happstack.Server.Monads
import Happstack.Server
import Control.Monad.IO.Class  (liftIO)
import System.Locale
import MasterTemplate
import MakeElements
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Scripts

testString1 = [["MAT137","","","",""],["","","CSC148","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","CSC165","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","","ECO100"]]
testString2 = [["M","","W","","F"],["M","","","",""],["","","W","","F"],["","","","","F"]]
testString = [["MAT137","","","",""],["","","CSC148","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","CSC165","",""],["","","","",""],["","SOC101","","CSC236",""],["","","","",""],["","","","",""],["","","MAT223","","ECO100"]]
testAllEventsString = [["MAT137","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","CSC165","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""]]

-- List of the subjects
names :: [[String]] -> [String]
names courses = concat [[subject | subject <- courseWeek, not(null subject)] | courseWeek <- courses]

-- The start time for each subject [[String,String,String,String,String]] -> [(String,[String,String,String,String,String]),]
startTimes :: [[String]] -> [(String,String)]
startTimes courses = concat(matchingTime(zip (courses) ([show x ++ ":00:00 AM"| x <- [8..11]] ++ [show x ++ ":00:00 PM"| x <- ([12] ++ [1..9])])))

-- The ending time for each subject [[String,String,String,String,String]] -> [([String,String,String,String,String], String)]
endTimes :: [[String]] -> [(String,String)]
endTimes courses = concat(matchingTime(zip (courses) ([show x ++ ":00:00 AM"| x <- [9..11]] ++ [show x ++ ":00:00 PM"| x <- ([12] ++ [1..10])])))

-- Match the time with
matchingTime :: [([String],String)] -> [[(String,String)]]
matchingTime timeCourses = justValid [zip (fst timeCourse) [snd timeCourse | x <- [0..7]] | timeCourse <- timeCourses]

-- Just take the valid tuples that have actual subjects intead of empty strings
justValid :: [[(String,String)]] -> [[(String, String)]]
justValid timeCoursesList = let notNull x = not (null x) in filter notNull [[timeCourse | timeCourse <- timeCourses, not(null(fst(timeCourse)))] | timeCourses <- timeCoursesList]

{- Output file:
"Subject, start date, start time, end date, end time, all day event, description, location, private
MAT137,09/14/15,8:00:00 AM,09/14/15,9:00:00 AM,False,MAT137,tba,True
29 events more

Input parameters
[("MAT137","8:00:00 AM"),..] -> [("MAT137","9:00:00 AM"),..] -> [[2015-09-14,2015-09-21,..],..]
-}

matchData :: [(String,String)] -> [(String,String)] -> [[Day]] -> [String]
matchData start end date = concat [eventsByCourse (start !! i) (end !! i) (date !! i) | i <- [0..x]]
    where
    x = (length start) - 1

eventsByCourse :: (String,String) -> (String,String) -> [Day] -> [String]
eventsByCourse start end date =  [fst start ++ "," ++ format byDate ++ "," ++ snd start ++ "," ++ format byDate ++ "," ++ snd end ++ ",False," ++ fst end ++ ",tba,True"| byDate <- date] 

format :: Day -> String
format date = formatTime defaultTimeLocale "%D" date

-- EVENTS START END DATE

-- First day of classes will be on September, September 14.
eventDays :: [[String]] -> [String] 
eventDays courses = allDays [giveDay byWeek| byWeek <- courses]

-- Create a list with all the days
allDays :: [[String]] -> [String] 
allDays listDays = [jutsDays | jutsDays <- concat listDays, not(null jutsDays)]

-- Get the day for each course 
giveDay :: [String] -> [String]
giveDay coursesWeek = [if null(coursesWeek !! i) then "" else day i  | i <- [0,1,2,3,4]]

-- Give the appropriate day for the course given based on its position
day :: Int -> String 
day 0 = "M"
day 1 = "T"
day 2 = "W"
day 3 = "R"
day 4 = "F"

-- Takes data from event days
startDate :: [[String]] -> [[Day]]
startDate courses = [generateDates days | days <- eventDays courses]

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

endDate :: [[String]] -> [[Day]]
endDate courses = startDate courses

test :: [[String]] -> String
test courses = toCSV(matchData (startTimes courses) (endTimes courses) (startDate courses))

{- Output file:
"Subject, start time, end time
MAT137,8:00:00 AM,9:00:00 AM
CSC148,9:00:00 AM,10:00:00 AM
CSC165,4:00:00 PM,5:00:00 PM
ECO100,9:00:00 PM,10:00:00 PM"
-}
toCSV :: [String] -> String
toCSV eventsData = unlines ([title] ++ eventsData)
    where
    title = "Subject,Start Date,Start Time,End Date,End Time,All Day Event,Description,Location,Private"

getCsvFile :: String -> String -> String
getCsvFile courses session = toCSV(matchData (startTimes coursesWeekly) (endTimes coursesWeekly) (startDate coursesWeekly))
    where
    coursesWeekly = splitCourses courses session

splitCourses :: String -> String -> [[String]]
splitCourses courses session = partition5 $ splitOn "_" courses
    where
    partition5 [] = []
    partition5 lst = take 5 lst : partition5(drop 5 lst)

getCalendar :: String -> String -> IO Response
getCalendar courses session = return $ toResponse(getCsvFile courses session)

-- | Returns a CSV file of events as requested by the user.
calendarResponse :: String -> String -> ServerPart Response
calendarResponse courses session =
    liftIO $ getCalendar courses session
    ok $ toResponse $
        masterTemplate "Courseography - Calendar"
                    [H.meta ! A.name "keywords"
                            ! A.content "",
                            timetableLinks
                    ]
                    timetableScripts

{-getCalendar :: String -> String -> IO Response IO ()
getCalendar courses session = simpleHTTP nullConf $ ok (toResponse(test testString))
return $to Response
{- do return $ createJSONResponse(getCalendar courses session)
ok $ toResponse $
notFound $ toResponse $

IO ()
ServerPartT IO ()
-}

-- | Returns a CSV file of events as requested by the user.
calendar :: String -> String -> ServerPart Response
calendar courses session =
    liftIO $ getCalendar courses session-}