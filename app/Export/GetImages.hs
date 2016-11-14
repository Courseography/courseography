{-# LANGUAGE ScopedTypeVariables #-}
module Export.GetImages
    (getActiveGraphImage, getTimetableImage, randomName, getActiveTimetable) where

import Export.TimetableImageCreator (renderTable, renderTableHelper, times)
import qualified Data.Map as M
import System.Random
import Svg.Generator
import Export.ImageConversion
import Happstack.Server (Request, rqCookies, cookieValue, Cookie)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Database.CourseQueries (getLectureTime, getTutorialTime)
import Database.Tables as Tables
import Data.List (partition)
import Database.Persist.Sqlite (runSqlite, runMigration)
import Config (databasePath)
import Data.Fixed (mod')

-- | If there is an active graph available, an image of that graph is created,
-- otherwise the Computer Science graph is created as a default.
-- Either way, the resulting graph's .svg and .png names are returned.
getActiveGraphImage :: Request -> IO (String, String)
getActiveGraphImage req = do
    let cookies = M.fromList $ rqCookies req
        graphName =
            replace "-" " " $
                maybe "Computer-Science" cookieValue (M.lookup "active-graph" cookies)
    getGraphImage graphName (M.map cookieValue cookies)

-- | If there are selected lectures available, an timetable image of
-- those lectures in specified session is created.
-- Otherwise an empty timetable image is created as default.
-- Either way, the resulting image's .svg and .png names are returned.
getActiveTimetable :: Request -> String -> IO (String, String)
getActiveTimetable req termSession = do
    -- get cookie value of "selected-lectures" from browser
    let cookies :: M.Map String Cookie = M.fromList $ rqCookies req
        coursecookie = maybe "" cookieValue $ M.lookup "selected-lectures" cookies
        (lecture, tutorial) = parseCourseCookie coursecookie termSession
    (lecTimes, tutTimes) <- getTimes (lecture, tutorial)
    let schedule' = getScheduleByTime lecture tutorial lecTimes tutTimes
    print schedule'
    generateTimetableImg schedule' termSession

-- | Parses cookie string and returns two lists of information about courses
-- in the format of (code, section, session).
-- One for lecture, the other for tutorial.
parseCourseCookie :: String -> String -> ([(String, String, String)], [(String, String, String)])
parseCourseCookie "" _ = ([], [])
parseCourseCookie s termSession =
  let lecAndTut = map (splitOn "-") $ splitOn "_" s
      (lecture, tutorial) = partition isLec lecAndTut
      -- get lecture and tutorial in this session
      [lectureOfSession, tutorialOfSession] = map (filter (\x -> or ([(x !! 2 !! 0) == (head termSession), (x !! 2 !! 0) == 'Y']))) [lecture, tutorial]
      lecture' = map (convertLec . list2tuple) lectureOfSession
      tutorial' = map (convertTut . list2tuple) tutorialOfSession
  in (lecture', tutorial')
  where isLec x = (x !! 1 !! 0) == 'L'

list2tuple :: [String] -> (String, String, String)
list2tuple [a, b, c] = (a, b, c)
list2tuple _ = undefined

convertLec :: (String, String, String) -> (String, String, String)
convertLec (lecCode, lecSection, lecSession) =
  (lecCode,
   take 1 lecSection ++ "EC-" ++ drop 1 lecSection,
   lecSession)

convertTut :: (String, String, String) -> (String, String, String)
convertTut (tutCode, tutSection, tutSession) =
  (tutCode,
   take 1 tutSection ++ "UT-" ++ drop 1 tutSection,
   tutSession)

-- | Queries the database for times regarding all lectures and tutorials,
-- returns two lists of list of Time.
getTimes :: ([(String, String, String)], [(String, String, String)]) -> IO ([[Time]], [[Time]])
getTimes (selectedLectures, selectedTutorials) = runSqlite databasePath $ do
  runMigration migrateAll
  lecTimes <- mapM getLectureTime selectedLectures
  tutTimes <- mapM getTutorialTime selectedTutorials
  return (lecTimes, tutTimes)

-- | Creates a schedule.
-- It takes information about lectures and tutorials and their corresponding time.
-- Courses are added to schedule, based on their days and times.
getScheduleByTime :: [(String, String, String)] -> [(String, String, String)] -> [[Time]] -> [[Time]] -> [[String]]
getScheduleByTime lecture tutorial lecTimes tutTimes =
  let lecture_times = zip lecture lecTimes
      tutorial_times = zip tutorial tutTimes
      allTimes = lecture_times ++ tutorial_times
      schedule = replicate 13 $ replicate 5 ""
  in foldl addCourseToSchedule schedule allTimes

-- | Take a list of Time and returns a list of tuples that correctly index
-- into the 2-D table (for generating the image)
convertTimeToArray :: [Time] -> [(Int, Int)]
convertTimeToArray = map (\x -> (floor $ timeField x !! 0 , floor $ timeField x !! 1 - 8))

addCourseToSchedule :: [[String]] -> ((String, String, String), [Time]) -> [[String]]
addCourseToSchedule schedule (course, courseTimes) =
  let time' = filter (\t-> (mod' (timeField t !! 1) 1) == 0) courseTimes
      timeArray = convertTimeToArray time'
  in foldl (addCourseHelper course) schedule timeArray

-- | Appends information of course to the current schedule for specified day and time.
-- Returns new schedule.
addCourseHelper :: (String, String, String) -> [[String]] -> (Int, Int) -> [[String]]
addCourseHelper (courseCode, courseSection, courseSession) currentSchedule (day, courseTime) =
  let time_schedule = currentSchedule !! courseTime
      current_schedule = if (null $ time_schedule !! day) then (courseCode++courseSession++" "++courseSection) else (time_schedule !! day ++ ("&"++courseCode++courseSession++" "++courseSection))
      time_schedule' = (take day time_schedule) ++ [current_schedule] ++ (drop (day + 1) time_schedule)
  in (take courseTime currentSchedule) ++ [time_schedule'] ++ (drop (courseTime+ 1) currentSchedule)

-- | Creates an timetable image based on schedule, and returns the name of the svg
-- used to create the image and the name of the image
generateTimetableImg :: [[String]] -> String -> IO(String, String)
generateTimetableImg schedule courseSession = do
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    renderTableHelper svgFilename (zipWith (:) times schedule) courseSession
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getGraphImage :: String -> M.Map String String -> IO (String, String)
getGraphImage graphName courseMap = do
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    buildSVG graphName courseMap svgFilename True
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getTimetableImage :: String -> String -> IO (String, String)
getTimetableImage courses termSession = do
    -- generate 2 random names
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    renderTable svgFilename courses termSession
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Generate a string containing random integers
randomName :: IO String
randomName = do
    gen <- newStdGen
    let (rand, _) = next gen
    return (show rand)
