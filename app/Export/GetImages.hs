{-|
    Module      : Export.GetImages
    Description : Defines functions for creating images from graphs and
                  timetables.

Defines functions for creating images from graphs and timetables, most
functions write to the svg and png files given their file or directory paths.
-}
module Export.GetImages
    (getActiveTimetable, writeActiveGraphImage) where

import Config (runDb)
import Data.Aeson (decodeStrictText)
import Data.Char (isAlphaNum)
import Data.Fixed (mod')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import Database.Tables as Tables
import Export.ImageConversion (withImageFile)
import Export.TimetableImageCreator (renderTableHelper, times)
import Models.Meeting (getMeetingTime)
import Svg.Generator (buildSVG)
import System.FilePath ((</>))
import System.IO (Handle)

-- | If there is an active graph available, an image of the active graph is written,
-- otherwise the Computer Science graph is written as a default.
writeActiveGraphImage :: T.Text -> Handle -> IO ()
writeActiveGraphImage graphInfo svgHandle = do
    let graphInfoMap = fromMaybe M.empty $ decodeStrictText graphInfo :: M.Map T.Text T.Text
        graphName = fromMaybe "Computer-Science" $ M.lookup "active-graph" graphInfoMap
    getGraphImage graphName graphInfoMap svgHandle

-- | If there are selected lectures available, a timetable image of
-- those lectures in specified session is created.
-- Otherwise an empty timetable image is created as default.
getActiveTimetable :: T.Text -> T.Text -> FilePath -> IO FilePath
getActiveTimetable selectedCourses termSession tempDir = do
    let selectedMeetings = parseSelectedCourses selectedCourses termSession
    mTimes <- getTimes selectedMeetings
    let schedule = getScheduleByTime selectedMeetings mTimes
    generateTimetableImg schedule termSession tempDir

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

-- | Creates a timetable image based on schedule.
generateTimetableImg :: [[[T.Text]]] -> T.Text -> FilePath -> IO FilePath
generateTimetableImg schedule courseSession tempDir = do
    let session = filter isAlphaNum (T.unpack courseSession)
        sessionStem = if null session then "session" else session
        pngPath = tempDir </> (sessionStem ++ ".png")
        svgText = renderTableHelper (zipWith (:) times schedule) courseSession
    withImageFile pngPath $ \hin ->
        LTIO.hPutStr hin svgText
    return pngPath

-- | Builds the graph svg, given file handler of the svg
getGraphImage :: T.Text -> M.Map T.Text T.Text -> Handle -> IO ()
getGraphImage graphName courseMap svgHandle = do
    buildSVG graphName courseMap svgHandle True
