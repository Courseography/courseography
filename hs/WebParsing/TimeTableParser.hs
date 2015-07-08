{-# LANGUAGE OverloadedStrings #-}

module WebParsing.TimeTableParser where

import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Persist.Sqlite
import Data.List
import qualified Data.Text as T
import Data.Maybe
import Database.Tables as Tables
import Database.CourseInsertion
import WebParsing.HtmlTable
import WebParsing.ParsingHelp
import WebParsing.TimeConverter
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Config (databasePath)

-- | used as an intermediate container while extracting lecture and tutorial information
-- from the table. Is is later converted into lecture or tutorial records by examinig the
-- first letter of the section.
data CourseSlot =
    CourseSlot { slotSection :: T.Text,
                 slotTime_str :: T.Text,
                 slotInstructor :: T.Text
               } deriving Show

timetableUrl :: String
timetableUrl = "http://www.artsandscience.utoronto.ca/ofr/timetable/winter/"

-- | a list of pages that contain special formatting, are dealt with seperately and removed
-- from main list.
specialCases :: [String]
specialCases = ["assem.html",
                "online.html",
                "academics-and-registration"]

-- | extracts a list of department page names from the main website
getDeptList :: [Tag String] -> [String]
getDeptList tags =
    let deptList = filter (tagOpen (=="a") isHref) tags
        notDepts = filter (\str -> (length str) < 20) (map getAttribute deptList)
    in nub $ filter (\dept -> not (dept `elem` specialCases)) notDepts
    where
      isHref [("href", _)] = True
      isHref _ = False
      getAttribute (TagOpen _ [(_, link)]) = link

-- | if a row contains "NOTE" we add 3 empty 'cells' to the beginning
--used to deal with corner case found in "csc.html"
expandNote :: [T.Text] -> [T.Text]
expandNote row =  if ( (T.take 4 (head row)) == "NOTE" )
                  then ["", "", ""] ++ (tail row)
                  else row

-- | takes in a department pagversace versaceversace versacee name, extracts the html table, partitions into a list of all
-- information related to a single course, and inserts the resulting tutorials and lectures
-- into the database.
getDeptTimetable :: MonadIO m => String -> ReaderT SqlBackend m ()
getDeptTimetable url = do
    rsp <- liftIO $ simpleHTTP (getRequest $ timetableUrl ++ url)
    body <- liftIO $ getResponseBody rsp
    let rawSoup = map cleanTag (parseTags (T.pack body))
        toLower = if (url == "online.html") then map lowerTag rawSoup else rawSoup
        table = dropAround  (tagOpen (=="table") (\_ -> True)) (tagClose (=="table")) toLower
    mapM_ (\(pos, course) -> processCourseTable (foldl (\c p -> expandTable c "" p) course pos)) (toCells table)
    --print toLower--were running into an empty list while printing out the final results-- look into this tomorrow
    where
        cleanTag (TagText str) = TagText (T.strip (replaceAll ["\r\n"] "" str))
        cleanTag str = str

-- | partitions the html table into a 2d list of cells. Does not account for cells that take
-- up more than one row or column.
toCells :: [Tag T.Text] -> [ ([Pos], [[T.Text]] ) ]
toCells tags =
    let row = partitions (tagOpen (=="tr") (\_ -> True)) tags
        rowsColumns  =  map (partitions (tagOpen (== "td") (\_ -> True))) row
          --courseRows groups cells by the courses they are contained in.
        removedEmpty = filter (not . null) rowsColumns
        dropLastCell = map init removedEmpty
        courseRows = partitions (\courseRow -> any (isCourse . fromTagText) (filter isTagText (head courseRow))) dropLastCell
        --foreach group of cells rep. courses, extracts the spans.
        courseSpans = map extractSpans courseRows -- [[Pos]]
        --following two lines takes out everything but text in each cell.
        filterCells = map (map (map (filter isTagText))) courseRows
        textCells =   map (map (map (\x -> T.concat (map fromTagText x))))  filterCells
    in (zip courseSpans textCells)

-- | adds a tutorial to the given session
addTutorial :: Session -> Tutorial -> Session
addTutorial sesh tut = sesh {tutorials = tut:tutorials sesh}

-- | adds a lecture to the given session
addLecture :: Session -> Lecture -> Session
addLecture sesh lec = sesh {lectures = lec:(lectures sesh)}

-- | extracts the required information from a row of cells and places it into a CourseSlot
--   if given a CourseSlot as input, it updates the time only. Otherwise updates time and
--   section
updateSlot :: [T.Text] -> Maybe CourseSlot -> Maybe CourseSlot
updateSlot row Nothing =
    if (isCancelled row) || length row < 8
    then Nothing
    else let timestr = T.takeWhile (/= ' ') (row !! 5)
             in Just CourseSlot { slotSection    = T.take 5 (row !! 3),
                                  slotTime_str   = timestr,
                                  slotInstructor = (row !! 7) }
updateSlot row (Just slot) =
    if (isCancelled) row || length row < 8
    then Just slot
    else let newTime = T.takeWhile (/= ' ') (row !! 5)
         in (Just slot {slotTime_str = (T.append newTime (T.append " " (slotTime_str slot)))})


-- | takes in cells representing a course, and recursively places lecture and tutorial info
-- into courseSlots.
parseCourse :: [[T.Text]] -> Maybe CourseSlot -> [Maybe CourseSlot] -> [Maybe CourseSlot]
parseCourse [] slot slots = slot:slots
parseCourse course Nothing slots =
    let row = head course
        rest = tail course
    in parseCourse rest (updateSlot row Nothing) slots
parseCourse course slot slots =
    let row = head course
        rest = tail course
    in if ((row !! 3) == "")
       then parseCourse rest (updateSlot row slot) slots
       else parseCourse rest (updateSlot row Nothing) (slot:slots)

-- | converts a courseSlot into a lecture
makeLecture :: CourseSlot -> Lecture
makeLecture slot =
    Lecture { extra = 0,
              section = (slotSection slot),
              cap = 0,
              time_str = (slotTime_str slot),
              time = concatMap makeTimeSlots (T.split (== ' ') (slotTime_str slot)),
              instructor = (slotInstructor slot),
              enrol = Nothing,
              wait = Nothing }

-- | converts a single courseSlot into a tutorial
makeTutorial :: CourseSlot -> Tutorial
makeTutorial slot =
    Tutorial {tutorialSection = Just (slotSection slot),
              times = concatMap makeTimeSlots (T.split (== ' ') (slotTime_str slot)),
              timeStr = (slotTime_str slot)}

-- | returns true if the courseSlot is housing a lecture, false otherwise.
isLecture :: CourseSlot -> Bool
isLecture slot = T.head (slotSection slot) == 'L'

-- | inserts a single courseSlot into a session
insertSession :: Session -> CourseSlot -> Session
insertSession sesh slot =
    if isLecture slot
    then sesh {lectures = (makeLecture slot):(lectures sesh)}
    else sesh {tutorials = (makeTutorial slot):(tutorials sesh)}

-- | inserts a list of courseSlots into a session, first converting them into lectures
--or tutorials.
makeSession :: [CourseSlot] -> Session
makeSession slots =
    let newSession = Session {lectures = [], tutorials = []}
    in foldl insertSession newSession slots

-- | takes in cells representing a single course, and inserts the lecture tutorial info
--into the database
processCourseTable :: MonadIO m => [[T.Text]] -> ReaderT SqlBackend m ()
processCourseTable course = do
    let session = (head course) !! 1
        code = T.take 8 ((head course) !! 0)
        slots = filter isJust (parseCourse course Nothing [])
        justSlots = map fromJust slots
        sesh = makeSession justSlots
    setTutorialEnrolment code (containsTut sesh)
    setPracticalEnrolment code (containsPrac sesh)
    mapM_ (insertLecture session code) (lectures sesh)
    mapM_ (insertTutorial session code) (tutorials sesh)
    liftIO $ print code
    where
        containsTut sesh = any (maybe False (T.isPrefixOf "T") . tutorialSection) $ tutorials sesh
        containsPrac sesh = any (maybe False (T.isPrefixOf "P") . tutorialSection) $ tutorials sesh

main :: IO ()
main = do
    rsp <- simpleHTTP (getRequest $ timetableUrl ++ "sponsors.htm")
    body <- getResponseBody rsp
    let _ = getDeptList $ parseTags body

    runSqlite databasePath $ do
        runMigration migrateAll
        getDeptTimetable "glaf.html"
    --mapM_ getDeptTimetable depts

parseTT :: IO ()
parseTT = do
    rsp <- simpleHTTP (getRequest timetableUrl)
    body <- getResponseBody rsp
    let depts = getDeptList $ parseTags body

    runSqlite databasePath $ do
        runMigration migrateAll
        mapM_ getDeptTimetable depts
