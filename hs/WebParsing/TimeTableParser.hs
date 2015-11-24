{-# LANGUAGE OverloadedStrings #-}

module WebParsing.TimeTableParser where

import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Persist.Sqlite
import Data.List
import Data.Char (isUpper)
import qualified Data.Text as T
import Data.Maybe
import Database.Tables as Tables
import Database.CourseInsertion
import WebParsing.HtmlTable
import WebParsing.ParsingHelp
import WebParsing.TimeConverter
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Config (databasePath, timetableUrl)
import Database.Tables (Department(Department))

parseTT :: IO ()
parseTT = do
    rsp <- simpleHTTP (getRequest timetableUrl)
    body <- getResponseBody rsp

    let depts = getDeptList $ parseTags body
        deptCode = getDeptCodes $ parseTags body
        deptName = getDeptName $ parseTags body
        deptAssocList = init $ constructAssociationList deptName deptCode

    runSqlite databasePath $
        mapM_ getDeptTimetable depts
    runSqlite databasePath $ 
        mapM_ insertDepartment deptAssocList

-- | List of unwanted words to remove when getting department codes.
wordsToRemove :: [String]
wordsToRemove = ["courses",
                "courses]", 
                "includes",
                "and",
                "-",
                "[",
                "]",
                ""]

-- | Extracts a list of lists of department codes from the main website
getDeptCodes :: [Tag String] -> [[T.Text]]
getDeptCodes tags = 
    let tagsText = map fromTagText $ filter isTagText tags
        -- filterDeptInfo returns only the strings that contain dept codes
        filterDeptInfo = map (charInString '[') tagsText
        removedEmptyStr = filter (\x -> x /= "") filterDeptInfo
        -- removedChars gets substring that contains dept codes
        removedChars = map (removeCharsBefore '[') removedEmptyStr
        listOfWords = map words removedChars
        relevantWords = map (filter (`notElem` wordsToRemove)) listOfWords
        -- deptCodesStr removes words from wordsToRemove to isolate dept Codes
        deptCodesStr = map (filter (\x -> all isUpper x == True)) relevantWords
        deptCodesText = map (map T.pack) deptCodesStr
    in deptCodesText

-- | Extracts a list of department names from the main website
getDeptName :: [Tag String] -> [T.Text]
getDeptName tags = 
    let tagsText = map fromTagText $ filter isTagText tags
        containsBrackLeft = filter (\x -> '[' `elem` x) tagsText
        convertToStr = map T.pack containsBrackLeft
        deptNamesText = map head $ map (T.splitOn (T.pack "[")) convertToStr
    in deptNamesText

-- | Takes a Char and a String and checks whether that char is in the String. If it
-- is, it returns the original string, otherwise it returns an empty string.
charInString :: Char -> String -> String 
charInString _ [] = ""
charInString y (x:xs) 
    | y == x = (x:xs)
    | otherwise = charInString y xs  

-- | Returns the chars after the first instance of the Char input from the given String.
removeCharsBefore :: Char -> String -> String
removeCharsBefore _ [] = ""
removeCharsBefore y (x:xs)
    | y == x = xs
    | otherwise = (x:xs)

-- | Constructs an association list given a list of T.Text and a list of lists of T.Text.
constructAssociationList :: [T.Text] -> [[T.Text]] -> [([T.Text], T.Text)]
constructAssociationList _ [] = []
constructAssociationList [] _ = []
constructAssociationList (x:xs) (y:ys) = [(y, x)] ++ constructAssociationList xs ys

-- | Inserts the Department association list in the database.
insertDepartment :: MonadIO m => ([T.Text], T.Text) -> ReaderT SqlBackend m ()
insertDepartment (code, name) = 
    Database.Persist.Sqlite.insert_ (Department code name)

-- | Used as an intermediate container while extracting lecture and tutorial information
-- from the table. Is is later converted into lecture or tutorial records by examinig the
-- first letter of the section.
data CourseSlot = CourseSlot {
    slotCode :: T.Text,
    slotSession :: T.Text,
    slotSection :: T.Text,
    slotTimeStr :: T.Text,
    slotInstructor :: T.Text
    } deriving Show

-- | A list of pages that contain special formatting, are dealt with seperately and removed
-- from main list.
specialCases :: [String]
specialCases = ["assem.html",
                "online.html",
                "academics-and-registration"]

-- | Extracts a list of department page names from the main website
getDeptList :: [Tag String] -> [String]
getDeptList tags =
    let deptList = filter (tagOpen (== "a") isHref) tags
        notDepts = filter (\str -> length str < 20) (map getAttribute deptList)
    in nub $ filter (`notElem` specialCases) notDepts
    where
        isHref [("href", _)] = True
        isHref _ = False
        getAttribute (TagOpen _ [(_, link)]) = link

-- | If a row contains "NOTE" we add 3 empty 'cells' to the beginning
-- used to deal with corner case found in "csc.html"
expandNote :: [T.Text] -> [T.Text]
expandNote row
    | T.take 4 (head row) == "NOTE" = ["", "", ""] ++ tail row
    | otherwise = row

-- | Takes in a department page name, extracts the html table, partitions into a list of all
-- information related to a single course, and inserts the resulting tutorials and lectures
-- into the database.
getDeptTimetable :: MonadIO m => String -> ReaderT SqlBackend m ()
getDeptTimetable url = do
    rsp <- liftIO $ simpleHTTP (getRequest $ timetableUrl ++ url)
    body <- liftIO $ getResponseBody rsp
    let rawSoup = map cleanTag (parseTags (T.pack body))
        toLower = if url == "online.html" then map lowerTag rawSoup else rawSoup
        table = dropAround  (tagOpen (=="table") (const True)) (tagClose (=="table")) toLower
    mapM_ (\(pos, course) -> processCourseTable (foldl (\c p -> expandTable c "" p) course pos)) (toCells table)
    -- TODO: What does the comment below this one mean?
    --print toLower--were running into an empty list while printing out the final results-- look into this tomorrow
    where
        cleanTag (TagText str) = TagText (T.strip (replaceAll ["\r\n"] "" str))
        cleanTag str = str

-- | Partitions the html table into a 2d list of cells. Does not account for cells that take
-- up more than one row or column.
toCells :: [Tag T.Text] -> [([Pos], [[T.Text]])]
toCells tags =
    let row = partitions (tagOpen (== "tr") (const True)) tags
        rowsColumns  =  map (partitions (tagOpen (== "td") (const True))) row
        -- courseRows groups cells by the courses they are contained in.
        removedEmpty = filter (not . null) rowsColumns
        dropLastCell = map init removedEmpty
        courseRows = partitions (any (isCourse . fromTagText) . filter isTagText . head) dropLastCell
        -- foreach group of cells rep. courses, extracts the spans.
        courseSpans = map extractSpans courseRows -- [[Pos]]
        -- following two lines takes out everything but text in each cell.
        filterCells = map (map (map (filter isTagText))) courseRows
        textCells =   map (map (map (T.concat . map fromTagText)))  filterCells
    in zip courseSpans textCells

-- | Adds a tutorial to the given session
addTutorial :: Session -> Tutorial -> Session
addTutorial session tut = session {tutorials = tut:tutorials session}

-- | Adds a lecture to the given session
addLecture :: Session -> Lecture -> Session
addLecture session lec = session { lectures = lec:lectures session }

-- | Extracts the required information from a row of cells and places it into a CourseSlot
-- if given a CourseSlot as input, it updates the time only. Otherwise updates time and
-- section
updateSlot :: [T.Text] -> Maybe CourseSlot -> T.Text -> T.Text -> Maybe CourseSlot
updateSlot row Nothing session code
    | isCancelled row || length row < 8 = Nothing
    | otherwise =
        let timestr = T.takeWhile (/= ' ') (row !! 5)
        in Just CourseSlot { slotCode       = code,
                             slotSession    = session,
                             slotSection    = T.take 5 (row !! 3),
                             slotTimeStr    = timestr,
                             slotInstructor = row !! 7 }
updateSlot row (Just slot) session code
    | isCancelled row || length row < 8 = Just slot
    | otherwise =
        let newTime = T.takeWhile (/= ' ') (row !! 5)
        in Just slot { slotCode    = code,
                       slotSession = session,
                       slotTimeStr = T.append newTime
                                          (T.append " " (slotTimeStr slot)) }

-- | Takes in cells representing a course, and recursively places lecture and tutorial info
-- into courseSlots.
parseCourse :: [[T.Text]] -> Maybe CourseSlot -> [Maybe CourseSlot] -> T.Text -> T.Text -> [Maybe CourseSlot]
parseCourse [] slot slots _ _ = slot:slots
parseCourse course Nothing slots session code =
    let row = head course
        rest = tail course
    in parseCourse rest (updateSlot row Nothing session code) slots session code
parseCourse course slot slots session code =
    let row = head course
        rest = tail course
    in if (row !! 3) == ""
       then parseCourse rest (updateSlot row slot session code) slots session code
       else parseCourse rest (updateSlot row Nothing session code) (slot:slots) session code

-- | Converts a courseSlot into a lecture
makeLecture :: CourseSlot -> Lecture
makeLecture slot = Lecture {
    lectureCode = slotCode slot,
    lectureSession = slotSession slot,
    lectureSection = (slotSection slot),
    lectureTimes = concatMap makeTimeSlots (T.split (== ' ') (slotTimeStr slot)),
    lectureCap = 0,
    lectureInstructor = (slotInstructor slot),
    lectureEnrol = 0,
    lectureWait = 0,
    lectureExtra = 0,
    lectureTimeStr = (slotTimeStr slot)
    }

-- | Converts a single courseSlot into a tutorial
makeTutorial :: CourseSlot -> Tutorial
makeTutorial slot = Tutorial {
    tutorialCode = slotCode slot,
    tutorialSession = slotSession slot,
    tutorialSection = Just (slotSection slot),
    tutorialTimes = concatMap makeTimeSlots (T.split (== ' ') (slotTimeStr slot))
    }

-- | Returns true if the courseSlot is housing a lecture, false otherwise.
isLecture :: CourseSlot -> Bool
isLecture slot = T.head (slotSection slot) == 'L'

-- | Inserts a single courseSlot into a session
insertSession :: Session -> CourseSlot -> Session
insertSession session slot
    | isLecture slot = session { lectures = makeLecture slot:lectures session }
    | otherwise = session { tutorials = makeTutorial slot:tutorials session }

-- | Inserts a list of courseSlots into a session, first converting them into lectures
-- or tutorials.
makeSession :: [CourseSlot] -> Session
makeSession slots =
    let newSession = Session {lectures = [], tutorials = []}
    in foldl insertSession newSession slots

-- | Takes in cells representing a single course, and inserts the lecture tutorial info
-- into the database
processCourseTable :: MonadIO m => [[T.Text]] -> ReaderT SqlBackend m ()
processCourseTable course = do
    let session = head course !! 1
        code = T.take 8 (head course !! 0)
        slots = filter isJust (parseCourse course Nothing [] session code)
        justSlots = map fromJust slots
        sesh = makeSession justSlots

    setTutorialEnrolment code (containsTut sesh)
    setPracticalEnrolment code (containsPrac sesh)
    mapM_ insert_ (lectures sesh)
    mapM_ insert_ (tutorials sesh)
    liftIO $ print code
    where
        containsTut sesh = any (maybe False (T.isPrefixOf "T") . tutorialSection) $ tutorials sesh
        containsPrac sesh = any (maybe False (T.isPrefixOf "P") . tutorialSection) $ tutorials sesh
