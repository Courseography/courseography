{-|
    Module      : Export.GetImages
    Description : Defines functions for creating images from graphs and
                  timetables.

Defines functions for creating images from graphs and timetables, most
functions return the name of the created svg and png files after creation.
-}
module Export.GetImages
    (getActiveGraphImage, getTimetableImage, randomName, getActiveTimetable) where

import Config (runDb)
import Data.Aeson (decode)
import Data.ByteString.Char8 as BC (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Fixed (mod')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Tables as Tables
import Export.ImageConversion
import Export.TimetableImageCreator (renderTable, renderTableHelper, times)
import Models.Meeting (getMeetingTime)
import Svg.Generator
import System.Random (genWord32, newStdGen)


-- | If there is an active graph available, an image of that graph is created,
-- otherwise the Computer Science graph is created as a default.
-- Either way, the resulting graph's .svg and .png names are returned.
getActiveGraphImage :: String -> IO (String, String)
getActiveGraphImage graphInfo = do
    let graphInfoMap = fromMaybe M.empty $ decode $ fromStrict $ BC.pack graphInfo :: M.Map T.Text T.Text
        graphName = fromMaybe "Computer-Science" $ M.lookup "active-graph" graphInfoMap
    getGraphImage graphName graphInfoMap


-- | If there are selected lectures available, an timetable image of
-- those lectures in specified session is created.
-- Otherwise an empty timetable image is created as default.
-- Either way, the resulting image's .svg and .png names are returned.
getActiveTimetable :: T.Text -> T.Text -> IO (String, String)
getActiveTimetable selectedCourses termSession = do
    let selectedMeetings = parseSelectedCourses selectedCourses termSession
    mTimes <- getTimes selectedMeetings
    let schedule = getScheduleByTime selectedMeetings mTimes
    generateTimetableImg schedule termSession

-- | Parses selected courses local storage and returns two lists of information about courses
-- in the format of (code, section, session).
parseSelectedCourses :: T.Text -> T.Text -> [(T.Text, T.Text, T.Text)]
parseSelectedCourses "" _ = []
parseSelectedCourses s termSession =
  let selectedMeetings = map (T.splitOn "-") $ T.splitOn "_" s
      meetingOfSession = filter (\x -> T.head (x !! 2) == T.head termSession || T.head (x !! 2) == 'Y') selectedMeetings
      selectedMeetings' = map list2tuple meetingOfSession
  in selectedMeetings'

list2tuple :: [T.Text] -> (T.Text, T.Text, T.Text)
list2tuple [a, b, c] = (a, b, c)
list2tuple _ = undefined

-- | Queries the database for times regarding all meetings (i.e. lectures, tutorials and praticals),
-- returns a list of list of Time.
getTimes :: [(T.Text, T.Text, T.Text)] -> IO [[Time]]
getTimes selectedMeetings = runDb $ mapM getMeetingTime selectedMeetings

-- | Creates a schedule.
-- It takes information about meetings (i.e. lectures, tutorials and praticals) and their corresponding time.
-- Courses are added to schedule, based on their days and times.
getScheduleByTime :: [(T.Text, T.Text, T.Text)] -> [[Time]] -> [[[T.Text]]]
getScheduleByTime selectedMeetings mTimes =
  let meetingTimes_ = zip selectedMeetings mTimes
      schedule = replicate 26 $ replicate 5 []
  in foldl addCourseToSchedule schedule meetingTimes_

-- | Take a list of Time and returns a list of tuples that correctly index
-- into the 2-D table (for generating the image).
-- TODO: Make this support half-hour times.
convertTimeToArray :: Time -> [(Int, Int)]
convertTimeToArray Time {weekDay=day, startHour=startTime, endHour=endTime} =
    [(floor day, row) | row <- [(floor startTime - 8)..(floor endTime - 8) - 1]]

addCourseToSchedule :: [[[T.Text]]] -> ((T.Text, T.Text, T.Text), [Time]) -> [[[T.Text]]]
addCourseToSchedule schedule (course, courseTimes) =
  let time' = filter (\t-> mod' (startHour t) 1 == 0) courseTimes
      timeArray = concatMap convertTimeToArray time'
  in foldl (addCourseHelper course) schedule timeArray

-- | Appends information of course to the current schedule for specified day and time.
-- Returns new schedule.
addCourseHelper :: (T.Text, T.Text, T.Text) -> [[[T.Text]]] -> (Int, Int) -> [[[T.Text]]]
addCourseHelper (courseCode, courseSection, courseSession) currentSchedule (day, courseTime) =
  let timeSchedule = currentSchedule !! courseTime
      newDaySchedule = timeSchedule !! day ++ [T.concat [courseCode, courseSession, " ", courseSection]]
      timeSchedule' = take day timeSchedule ++ [newDaySchedule] ++ drop (day + 1) timeSchedule
  in take courseTime currentSchedule ++ [timeSchedule'] ++ drop (courseTime + 1) currentSchedule

-- | Creates an timetable image based on schedule, and returns the name of the svg
-- used to create the image and the name of the image
generateTimetableImg :: [[[T.Text]]] -> T.Text -> IO (String, String)
generateTimetableImg schedule courseSession = do
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    renderTableHelper svgFilename (zipWith (:) times schedule) courseSession
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getGraphImage :: T.Text -> M.Map T.Text T.Text -> IO (String, String)
getGraphImage graphName courseMap = do
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    buildSVG graphName courseMap svgFilename True
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getTimetableImage :: T.Text -> T.Text -> IO (String, String)
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
    let (rand, _) = genWord32 gen
    return (show rand)
