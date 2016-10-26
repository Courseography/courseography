module Export.GetImages
    (getActiveGraphImage, getTimetableImage, randomName, getActiveTimetable) where

import Export.TimetableImageCreator (renderTable, renderTableHelper, times)
import qualified Data.Map as M
import System.Random
import Svg.Generator
import Export.ImageConversion
import Happstack.Server (Request, rqCookies, cookieValue)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Database.CourseQueries (getLectureTime, getTutorialTime)
import Database.Tables as Tables
import Control.Monad.IO.Class  (liftIO)
import Data.List (partition)
import Database.Persist.Sqlite (runSqlite, runMigration)
import Config (databasePath)

-- | If there is an active graph available, an image of that graph is created,
-- otherwise the Computer Science graph is created as a default.
-- Either way, the resulting graph's .svg and .png names are returned.
getActiveGraphImage :: Request -> IO (String, String)
getActiveGraphImage req = do
    let cookies = M.fromList $ rqCookies req  --  Map String Cookie
        graphName =
            replace "-" " " $
                maybe "Computer-Science" cookieValue (M.lookup "active-graph" cookies)  -- if M.lookup "active-graph" cookies is Nothing, then "Computer Science", else get cookieValue of coookie
    getGraphImage graphName (M.map cookieValue cookies)


-- =================================

-- data CourseInfo = CourseInfo {code :: String, section :: String, session :: String, time :: [Time]} deriving (Show)

-- | Get lectures selected by user from selected-lectures cookie. If DNE, assume no lecture seleted
-- get "CSC148H1-L0301-S_STA257H1-T0105-F_STA257H1-L0101-F_STA303H1-L0101-S_CSC148H1-T5101-F"
-- return [[Time {timeField = [2.0,18.0]},Time {timeField = [2.0,18.5]},Time {timeField = [2.0,19.0]},Time {timeField = [2.0,19.5]},Time {timeField = [2.0,20.0]},Time {timeField = [2.0,20.5]}],[Time {timeField = [0.0,14.0]},Time {timeField = [0.0,14.5]},Time {timeField = [0.0,15.0]},Time {timeField = [0.0,15.5]},Time {timeField = [2.0,14.0]},Time {timeField = [2.0,14.5]}],Just [Time {timeField = [2.0,10.0]},Time {timeField = [2.0,10.5]},Time {timeField = [0.0,10.0]},Time {timeField = [0.0,10.5]},Time {timeField = [4.0,10.0]},Time {timeField = [4.0,10.5]}], [Time {timeField = [4.0,11.0]},Time {timeField = [4.0,11.5]},Time {timeField = [4.0,12.0]},Time {timeField = [4.0,12.5]}]]
-- [["8:00","","","","",""],["9:00","","","","",""],["10:00","CSC108 (L)","","CSC108 (L)","","CSC108 (L)"],["11:00","","","","",""],["12:00","","","","",""],["1:00","","","","",""],["2:00","STA355 (L)","","STA355 (L)","",""],["3:00","STA355 (L)","","","",""],["4:00","","","","",""],["5:00","","","","",""],["6:00","","","","",""],["7:00","","","","",""],["8:00","","","","",""]]
getActiveTimetable :: Request -> String -> IO (String, String)
getActiveTimetable req session = do
    -- get cookie value of "selected-lectures" from browser
    let cookies = M.fromList $ rqCookies req  --  Map String Cookie
        coursecookie = maybe "" cookieValue $ M.lookup "selected-lectures" cookies
        (lecture, tutorial) = parseCourseCookie coursecookie session  -- ([CourseInfo], [CourseInfo])
    (lecTimes, tutTimes) <- getTimes (lecture, tutorial)
    let schedule' = getScheduleByTime lecTimes tutTimes
    (svgFilename, imageFilename) <- generateTimetableImg schedule' session
    return (svgFilename, imageFilename)


-- | Parse cookie string to two lists of courses, one for lecture, the other for tutorial
-- "CSC148H1-L5101-S_CSC148H1-T0501-S_STA355H1-L0101-F_CSC108H1-L0102-F"
-- [("STA355H1","LEC-0101","F"),("CSC108H1","LEC-0102","F")], [])
-- parseCourseCookie :: String -> String -> ([(String, String, String)], [(String, String, String)])
-- parseCourseCookie s session = let lecAndTut = map (splitOn "-") $ splitOn "_" s
--                                   (lecture, tutorial) = partition isLec lecAndTut
--                                   [lectureOfsession, tutorialOfsession] = map (filter (\x -> or ([(x !! 2 !! 0) == (head session), (x !! 2 !! 0) == 'Y']))) [lecture, tutorial]
--                                   lecture' = map (convertLec . list2tuple) lectureOfsession
--                                   tutorial' = map (convertTut . list2tuple) tutorialOfsession
--                               in (lecture', tutorial')
--                               where isLec x = (x !! 1 !! 0) == 'L'

parseCourseCookie :: String -> String -> ([CourseInfo], [CourseInfo])
parseCourseCookie s session = let lecAndTut = map (splitOn "-") $ splitOn "_" s
                                  (lecture, tutorial) = partition isLec lecAndTut
                                  -- get lecture and tutorial in this session
                                  [lectureOfsession, tutorialOfsession] = map (filter (\x -> or ([(x !! 2 !! 0) == (head session), (x !! 2 !! 0) == 'Y']))) [lecture, tutorial]
                                  lecture' = map (convertLec . list2tuple) lectureOfsession
                                  tutorial' = map (convertTut . list2tuple) tutorialOfsession
                              in (lecture', tutorial')
                              where isLec x = (x !! 1 !! 0) == 'L'

list2tuple :: [String] -> (String, String, String)
list2tuple [a, b, c] = (a, b, c)
list2tuple _ = undefined

-- ("CSC148H1","L5101","S") -> CourseInfo {code = "CSC148H1", section = "LEC-0501", session = "S", time = []} convert format to match database
convertLec :: (String, String, String) -> CourseInfo
convertLec (code, section, session) = CourseInfo {code = code, section = take 1 section ++ "EC-" ++ drop 1 section, session = session, time = [] }

-- ("CSC148H1","T0501","S") -> CourseInfo {code = "CSC148H1", section = "TUT-0501", session = "S", time = []} convert format to match database
convertTut :: (String, String, String) -> CourseInfo
convertTut (code, section, session) = CourseInfo {code = code, section = take 1 section ++ "UT-" ++ drop 1 section, session = session, time = [] }


-- | Get data from database for selected courses
getTimes :: ([CourseInfo], [CourseInfo]) -> IO ([CourseInfo], [CourseInfo])
getTimes (lecture, tutorial) = runSqlite databasePath $ do
    runMigration migrateAll
    lecTimes <- mapM getLectureTime lecture  -- [(String, String, String)] -> IO ([[Time]])
    tutTimes <- mapM getTutorialTime tutorial
    return (lecTimes, tutTimes)


-- | Generate scheduel in special format based on Times
getScheduleByTime :: [CourseInfo] -> [CourseInfo] -> [[String]]
getScheduleByTime lecTimes tutTimes = let allTimes = lecTimes ++ tutTimes
                                          schedule = replicate 13 $ replicate 5 ""
                                          schedule' = foldl (\acc x -> addCourseToSchedule x acc) schedule allTimes
                                      in schedule'

-- getScheduleByTime :: [(String, String, String)] -> [(String, String, String)] -> [[Time]] -> [[Time]] -> [[String]]
-- getScheduleByTime lecture tutorial lecTimes tutTimes = let lecture_times = zip lecture lecTimes
--                                                            tutorial_times = zip tutorial tutTimes
--                                                            all_times = lecture_times ++ tutorial_times
--                                                            schedule = replicate 13 $ replicate 5 ""
--                                                            schedule' = foldl (\acc x -> addCourseToSchedule x acc) schedule all_times
--                                                        in schedule'



-- CourseInfo {code = "STA355H1", section = "L0101", session = "F", time = [Time {timeField = [0.0,14.0]},Time {timeField = [0.0,14.5]},Time {timeField = [0.0,15.0]}]}
addCourseToSchedule :: CourseInfo -> [[String]] -> [[String]]
addCourseToSchedule courseInfo schedule
    | (time courseInfo) == [] = schedule
    | otherwise = foldl (\acc x -> addCourseHelper (code courseInfo) (section courseInfo) (session courseInfo) acc x) schedule (time courseInfo)


-- (("STA355H1","L0101","F"),Just [Time {timeField = [0.0,14.0]},Time {timeField = [0.0,14.5]},Time {timeField = [0.0,15.0]},Time {timeField = [0.0,15.5]},Time {timeField = [2.0,14.0]},Time {timeField = [2.0,14.5]}])
-- [["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""]]
-- addCourseToSchedule :: ((String, String, String), [Time]) -> [[String]] -> [[String]]
-- addCourseToSchedule (course, courseTimes) schedule = foldl (\acc x -> addCourseHelper course acc x) schedule courseTimes


--  "STA355H1" "L0101" "F"  Time {timeField = [0.0,14.0]}
addCourseHelper :: String -> String -> String -> [[String]] -> Time -> [[String]]
addCourseHelper code section session acc x = let [day, time'] = map floor $ timeField x
                                                 time = time' - 8
                                                 time_schedule = acc !! time
                                                 time_schedule' = (take day time_schedule) ++ [code++session++" "++section] ++ (drop (day + 1) time_schedule)
                                                 newacc = (take time acc) ++ [time_schedule'] ++ (drop (time + 1) acc)
                                             in newacc


-- ("STA355H1","L0101","F")  Time {timeField = [0.0,14.0]}
-- addCourseHelper :: (String, String, String) -> [[String]] -> Time -> [[String]]
-- addCourseHelper (code, section, session) acc x = let [day, time'] = map floor $ timeField x
--                                                      time = time' - 8
--                                                      time_schedule = acc !! time
--                                                      time_schedule' = (take day time_schedule) ++ [code++session++" "++section] ++ (drop (day + 1) time_schedule)
--                                                      newacc = (take time acc) ++ [time_schedule'] ++ (drop (time + 1) acc)
--                                                  in newacc


generateTimetableImg :: [[String]] -> String -> IO(String, String)
generateTimetableImg schedule session = do
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    renderTableHelper svgFilename (zipWith (:) times schedule) session
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- =================================

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
getTimetableImage courses session = do
    -- generate 2 random names
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    renderTable svgFilename courses session
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Generate a string containing random integers
randomName :: IO String
randomName = do
    gen <- newStdGen
    let (rand, _) = next gen
    return (show rand)
