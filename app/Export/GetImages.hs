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
-- Get lectures selected by user from selected-lectures cookie. If DNE, assume no lecture seleted
-- get "CSC148H1-L0301-S_STA257H1-T0105-F_STA257H1-L0101-F_STA303H1-L0101-S_CSC148H1-T5101-F"
-- return [Just [Time {timeField = [2.0,18.0]},Time {timeField = [2.0,18.5]},Time {timeField = [2.0,19.0]},Time {timeField = [2.0,19.5]},Time {timeField = [2.0,20.0]},Time {timeField = [2.0,20.5]}],Just [Time {timeField = [0.0,14.0]},Time {timeField = [0.0,14.5]},Time {timeField = [0.0,15.0]},Time {timeField = [0.0,15.5]},Time {timeField = [2.0,14.0]},Time {timeField = [2.0,14.5]}],Just [Time {timeField = [2.0,10.0]},Time {timeField = [2.0,10.5]},Time {timeField = [0.0,10.0]},Time {timeField = [0.0,10.5]},Time {timeField = [4.0,10.0]},Time {timeField = [4.0,10.5]}],Just [Time {timeField = [4.0,11.0]},Time {timeField = [4.0,11.5]},Time {timeField = [4.0,12.0]},Time {timeField = [4.0,12.5]}]]
-- [["8:00","","","","",""],["9:00","","","","",""],["10:00","CSC108 (L)","","CSC108 (L)","","CSC108 (L)"],["11:00","","","","",""],["12:00","","","","",""],["1:00","","","","",""],["2:00","STA355 (L)","","STA355 (L)","",""],["3:00","STA355 (L)","","","",""],["4:00","","","","",""],["5:00","","","","",""],["6:00","","","","",""],["7:00","","","","",""],["8:00","","","","",""]]
getActiveTimetable :: Request -> IO (String, String, String, String)
getActiveTimetable req = do
    let cookies = M.fromList $ rqCookies req  --  Map String Cookie
        coursecookie = maybe "" cookieValue $ M.lookup "selected-lectures" cookies
        (lectures, tutorials) = parseCourseCookie coursecookie
    -- liftIO $ print coursecookie
    -- liftIO $ print lectures
    -- liftIO $ print tutorials
    lecTimes <- mapM getLectureTime lectures  -- [[string]] -> IO ([Maybe [Time]])
    tutTimes <- mapM getTutorialTime tutorials
    let lecture_times = zip lectures lecTimes
        tutorial_times = zip tutorials tutTimes
        all_times = lecture_times ++ tutorial_times
        (fall_times, spring_times) = partition isFall all_times
        fall_schedule = replicate 13 $ replicate 5 ""
        spring_schedule = replicate 13 $ replicate 5 ""
        fall_schedule' = foldl (\acc x -> addCourseToSchedule x acc) fall_schedule fall_times
        spring_schedule' = foldl (\acc x -> addCourseToSchedule x acc) spring_schedule spring_times
    fallrand <- randomName
    springrand <- randomName
    let fallsvgFilename = fallrand ++ ".svg"
        fallimageFilename = fallrand ++ ".png"
        springsvgFilename = springrand ++ ".svg"
        springimageFilename = springrand ++ ".png"
    renderTableHelper fallsvgFilename (zipWith (:) times fall_schedule') "Fall"
    renderTableHelper springsvgFilename (zipWith (:) times spring_schedule') "Spring"
    createImageFile fallsvgFilename fallimageFilename
    createImageFile springsvgFilename springimageFilename
    return (fallsvgFilename, fallimageFilename, springsvgFilename, springimageFilename)
    where isFall (c, t) = last c == "F"
    


-- "CSC148H1-L5101-S_CSC148H1-T0501-S_STA355H1-L0101-F_CSC108H1-L0102-F"
-- ([["CSC148H1","L5101","S"],["STA355H1","L0101","F"],["CSC108H1","L0102","F"]],  [["CSC148H1","T0501","S"]])
parseCourseCookie :: String -> ([[String]], [[String]])
parseCourseCookie s = let lecAndTut = map (splitOn "-") $ splitOn "_" s
                          lectures = filter isLec lecAndTut
                          tutorials = filter isTut lecAndTut
                      in (lectures, tutorials)
                      where isTut x = (x !! 1 !! 0) == 'T'
                            isLec x = (x !! 1 !! 0) == 'L'


-- (["STA355H1","L0101","F"],Just [Time {timeField = [0.0,14.0]},Time {timeField = [0.0,14.5]},Time {timeField = [0.0,15.0]},Time {timeField = [0.0,15.5]},Time {timeField = [2.0,14.0]},Time {timeField = [2.0,14.5]}])
-- [["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""],["","","","",""]]
addCourseToSchedule :: ([String], Maybe [Time]) -> [[String]] -> [[String]]
addCourseToSchedule (_, Nothing) schedule = schedule
addCourseToSchedule (course, Just times) schedule = foldl (\acc x -> addCourseHelper course acc x) schedule times


addCourseHelper :: [String] -> [[String]] -> Time -> [[String]]
addCourseHelper [code, section, session] acc x = let [day, time'] = map floor $ timeField x
                                                     time = time' - 8
                                                     time_schedule = acc !! time
                                                     time_schedule' = (take day time_schedule) ++ [code++session++" "++section] ++ (drop (day + 1) time_schedule)
                                                     newacc = (take time acc) ++ [time_schedule'] ++ (drop (time + 1) acc)
                                                 in newacc


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
