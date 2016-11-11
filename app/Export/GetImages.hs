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


-- | Get lectures selected by user from selected-lectures cookie. If DNE, assume no lecture seleted
-- get "CSC148H1-L0301-S_STA257H1-T0105-F_STA257H1-L0101-F_STA303H1-L0101-S_CSC148H1-T5101-F"
-- return [[Time {timeField = [2.0,18.0]},Time {timeField = [2.0,18.5]},Time {timeField = [2.0,19.0]},Time {timeField = [2.0,19.5]},Time {timeField = [2.0,20.0]},Time {timeField = [2.0,20.5]}],[Time {timeField = [0.0,14.0]},Time {timeField = [0.0,14.5]},Time {timeField = [0.0,15.0]},Time {timeField = [0.0,15.5]},Time {timeField = [2.0,14.0]},Time {timeField = [2.0,14.5]}],Just [Time {timeField = [2.0,10.0]},Time {timeField = [2.0,10.5]},Time {timeField = [0.0,10.0]},Time {timeField = [0.0,10.5]},Time {timeField = [4.0,10.0]},Time {timeField = [4.0,10.5]}], [Time {timeField = [4.0,11.0]},Time {timeField = [4.0,11.5]},Time {timeField = [4.0,12.0]},Time {timeField = [4.0,12.5]}]]
-- [["8:00","","","","",""],["9:00","","","","",""],["10:00","CSC108 (L)","","CSC108 (L)","","CSC108 (L)"],["11:00","","","","",""],["12:00","","","","",""],["1:00","","","","",""],["2:00","STA355 (L)","","STA355 (L)","",""],["3:00","STA355 (L)","","","",""],["4:00","","","","",""],["5:00","","","","",""],["6:00","","","","",""],["7:00","","","","",""],["8:00","","","","",""]]
getActiveTimetable :: Request -> String -> IO (String, String)
getActiveTimetable req termSession = do
    -- get cookie value of "selected-lectures" from browser
    let cookies :: M.Map String Cookie = M.fromList $ rqCookies req
        coursecookie = maybe "" cookieValue $ M.lookup "selected-lectures" cookies
        (lecture, tutorial) = parseCourseCookie coursecookie termSession  -- ([CourseInfo], [CourseInfo])
    (lecTimes, tutTimes) <- getTimes (lecture, tutorial)
    let schedule' = getScheduleByTime lecTimes tutTimes
    print schedule'
    generateTimetableImg schedule' termSession


-- | Parse cookie string to two lists of courses, one for lecture, the other for tutorial
-- "CSC148H1-L5101-S_CSC148H1-T0501-S_STA355H1-L0101-F_CSC108H1-L0102-F"
-- [("STA355H1","LEC-0101","F"),("CSC108H1","LEC-0102","F")], [])
parseCourseCookie :: String -> String -> ([CourseInfo], [CourseInfo])
parseCourseCookie s termSession =
  let lecAndTut = map (splitOn "-") $ splitOn "_" s
      (lecture, tutorial) = partition isLec lecAndTut
      -- get lecture and tutorial in this session
      [lectureOfsession, tutorialOfsession] = map (filter (\x -> or ([(x !! 2 !! 0) == (head termSession), (x !! 2 !! 0) == 'Y']))) [lecture, tutorial]
      lecture' = map (convertLec . list2tuple) lectureOfsession
      tutorial' = map (convertTut . list2tuple) tutorialOfsession
  in (lecture', tutorial')
  where isLec x = (x !! 1 !! 0) == 'L'

list2tuple :: [String] -> (String, String, String)
list2tuple [a, b, c] = (a, b, c)
list2tuple _ = undefined

-- ("CSC148H1","L5101","S") -> CourseInfo {code = "CSC148H1", section = "LEC-0501", session = "S", time = []} convert format to match database
convertLec :: (String, String, String) -> CourseInfo
convertLec (lecCode, lecSection, lecSession) = CourseInfo {
  code = lecCode,
  section = take 1 lecSection ++ "EC-" ++ drop 1 lecSection,
  session = lecSession,
  time = []
  }

-- ("CSC148H1","T0501","S") -> CourseInfo {code = "CSC148H1", section = "TUT-0501", session = "S", time = []} convert format to match database
convertTut :: (String, String, String) -> CourseInfo
convertTut (tutCode, tutSection, tutSession) = CourseInfo {
  code = tutCode,
  section = take 1 tutSection ++ "UT-" ++ drop 1 tutSection,
  session = tutSession,
  time = []
  }


-- | Get data from database for selected courses
getTimes :: ([CourseInfo], [CourseInfo]) -> IO ([CourseInfo], [CourseInfo])
getTimes (selectedLectures, selectedTutorials) = runSqlite databasePath $ do
  runMigration migrateAll
  lecTimes <- mapM getLectureTime selectedLectures
  tutTimes <- mapM getTutorialTime selectedTutorials
  return (lecTimes, tutTimes)


-- | Generate scheduel in special format based on Times
getScheduleByTime :: [CourseInfo] -> [CourseInfo] -> [[String]]
getScheduleByTime lecTimes tutTimes =
  let allTimes = lecTimes ++ tutTimes
      schedule = replicate 13 $ replicate 5 ""
      schedule' = foldl addCourseToSchedule schedule allTimes
  in schedule'

-- CourseInfo {code = "STA355H1", section = "L0101", session = "F", time = [Time {timeField = [0.0,14.0]},Time {timeField = [0.0,14.5]},Time {timeField = [0.0,15.0]}]}
addCourseToSchedule :: [[String]] -> CourseInfo -> [[String]]
addCourseToSchedule schedule courseInfo
    | (time courseInfo) == [] = schedule
    | otherwise =
        let time' = filter (\t-> (mod' (timeField t !! 1) 1) == 0) (time courseInfo)
            timeArray = convertTimeToArray time'
            in foldl (addCourseHelper (code courseInfo) (section courseInfo) (session courseInfo)) schedule timeArray

-- | Take a list of Time and returns a list of tuples that correctly index into the 2-D table (for generating the image)
-- Time {timeField = [0.0,14.0]} -> (0, 6)
convertTimeToArray :: [Time] -> [(Int, Int)]
convertTimeToArray = map (\x -> (floor $ timeField x !! 0 , floor $ timeField x !! 1 - 8))


--  "STA355H1" "L0101" "F"  Time {timeField = [0.0,14.0]}
addCourseHelper :: String -> String -> String -> [[String]] -> (Int, Int) -> [[String]]
addCourseHelper courseCode courseSection courseSession currentSchedule (day, courseTime) =
  let time_schedule = currentSchedule !! courseTime
      current_schedule = if (null $ time_schedule !! day) then (courseCode++courseSession++" "++courseSection) else (time_schedule !! day ++ ("&"++courseCode++courseSession++" "++courseSection))
      time_schedule' = (take day time_schedule) ++ [current_schedule] ++ (drop (day + 1) time_schedule)
      newSchedule = (take courseTime currentSchedule) ++ [time_schedule'] ++ (drop (courseTime + 1) currentSchedule)
  in newSchedule

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
