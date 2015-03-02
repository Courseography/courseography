{-# LANGUAGE OverloadedStrings #-}

module WebParsing.TimeTableParser where

import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Persist
import Database.Persist.Sqlite
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as B
import Data.List.Utils
import Data.Maybe
import Database.Tables as Tables
import Database.JsonParser
import WebParsing.ParsingHelp
import Control.Monad.IO.Class

type Table = [[Tag T.Text]]

{--------------------------------------------------------------------------------------
-obtain list of department urls with getDeptList
-foreach department page, extract table and partition by course
  (getDeptTimeTable, toCells)
-foreach course, create a Session Record of all lectures and tutorials
  (updateSession, updateSLot, processCourseTable)
-foreach Session Record, add all lectures and tutorials into tables
--------------------------------------------------------------------------------------}

timetableUrl :: String
timetableUrl = "http://www.artsandscience.utoronto.ca/ofr/timetable/winter/"
{---------------------------------------------------------------------------------------
A courseSlot is a temporary storage Record that is used to generalize the information
stored in a lecture and a tutorial. Since the information of a single lecture or tutorial
is represented in a series of  
----------------------------------------------------------------------------------------}
data CourseSlot = 
  CourseSlot {
              --shared information
              slotSection :: T.Text,
              slotTime_str :: T.Text,
              --Lecture
              slotExtra :: Int,
              slotCap :: Int,
              slotTime :: [[Int]],
              slotInstructor :: T.Text,
              slotEnrol :: Maybe Int,
              slotWait :: Maybe Int
              }

{----------------------------------------------------------------------------------------
insantiates an 'empty' courseSLot
----------------------------------------------------------------------------------------}
emptySlot :: CourseSlot
emptySlot = CourseSlot {
  slotSection = "",
  slotTime_str = "",
  slotExtra = 0,
  slotCap = 0,
  slotTime = [],
  slotInstructor = "",
  slotEnrol = Nothing,
  slotWait = Nothing
}

--csc.html, assem.html, online.html,
{----------------------------------------------------------------------------------------
a list of pages that contain special formatting, are dealt with seperately and removed 
from main list.
----------------------------------------------------------------------------------------}
specialCases :: [String]
specialCases = ["assem.html",
                "csc.html",
                "online.html"]

{----------------------------------------------------------------------------------------
extracts a list of department page names from the main website
----------------------------------------------------------------------------------------}
getDeptList :: [Tag String] -> [String]
getDeptList tags = 
  let deptList = filter (tagOpen (=="a") isHref) tags
      notDepts = filter (\s -> (length s) < 20) (map getAttribute deptList)
  in nub $ filter (\dept -> not (dept `elem` specialCases)) notDepts
	where
		isHref [("href", _)] = True
		isHref _ = False
		getAttribute (TagOpen _ [(_, link)]) = link


expandNote :: [T.Text] -> [T.Text]
expandNote row =  if ( (T.take 4 (head row)) == "NOTE" )
                  then ["", "", ""] ++ (tail row)
                  else row

{----------------------------------------------------------------------------------------
takes in a department page name, extracts the html table, partitions into a list of all
information related to a single course, and inserts the resulting tutorials and lectures
into the database.
----------------------------------------------------------------------------------------}
getDeptTimetable :: String -> IO()
getDeptTimetable url = do
  rsp <- simpleHTTP (getRequest $ timetableUrl ++ url)
  body <- getResponseBody rsp
  let rawSoup = map cleanTag (parseTags (T.pack body))
      table = dropAround  (tagOpen (\x -> or [(x =="table"), (x=="TABLE")]) (\x -> True)) 
                          (tagClose (\x -> or [(x=="table"), (x=="TABLE")])) 
                          rawSoup
      cells = filter (\x ->  and [(x /= []),(length x) > 4, notCancelled (x !! 4)])  (toCells table)
      expandedNote = map expandNote cells
      courseCells = partitions (\row -> ((head row) /= "")) expandedNote
      sessions = map processCourseTable courseCells
  print url
  mapM_ processCourseTable courseCells
  --print rawSoup
  where
    cleanTag (TagText s) = TagText (T.strip (replaceAll ["\r\n"] "" s))
    cleanTag s = s
    notCancelled "" = True
    notCancelled str = (T.head str) /= 'C'

{----------------------------------------------------------------------------------------
partitions the html table into a 2d list of cells. Does not account for cells that take
up more than one row or column.
----------------------------------------------------------------------------------------}
toCells :: [Tag T.Text] -> [[T.Text]]
toCells tags = 
  let row = partitions (tagOpen (=="tr") (\x -> True)) tags
      rowsColumns =  map (partitions (tagOpen (== "td") (\x -> True))) row
      filterCells = map (map (filter isTagText)) rowsColumns
      textCells = map (map (map fromTagText)) filterCells
  in  map (map T.concat) textCells


{----------------------------------------------------------------------------------------
creates a tutorial from the information currently residing in the CourseSlot
----------------------------------------------------------------------------------------}
makeTutorial :: CourseSlot -> Maybe Tutorial -> Tutorial
makeTutorial slot Nothing =
  Tutorial {  tutorialSection = Just (slotSection slot),
              times = [],
              timeStr = (slotTime_str slot)
            }
makeTutorial slot (Just tutorial) =
  tutorial {timeStr = T.append (timeStr tutorial) (slotTime_str slot)}

{----------------------------------------------------------------------------------------
makes a lecture from the information currently residing in the courseSlot
----------------------------------------------------------------------------------------}
makeLecture :: CourseSlot -> Maybe Lecture -> Lecture
makeLecture slot Nothing = 
 Lecture { extra = (slotExtra slot),
              section = (slotSection slot),
              cap = (slotCap slot),
              time_str = (slotTime_str slot),
              time = (slotTime slot),
              instructor = (slotInstructor slot),
              enrol = (slotEnrol slot),
              wait = (slotWait slot)
            }
makeLecture slot (Just lec) = lec {time_str = T.append (time_str lec) (slotTime_str slot)}

{----------------------------------------------------------------------------------------
updates the courseSlot record with information from a given row
----------------------------------------------------------------------------------------}
updateSlot :: [T.Text] -> CourseSlot -> CourseSlot
updateSlot row slot =
  let partialUpdate = slot {
            slotTime_str = T.takeWhile (/= ' ') (row !! 5),
            slotInstructor = (row !! 7)}
  in  if (row !! 3) == "" --then no new section
      then partialUpdate
      else partialUpdate {slotSection = (row !! 3)}

{----------------------------------------------------------------------------------------
Takes in a 2-d list of 'cells' corresponding to a single course. Each lecture and tutori-
al can span more than a single row if it has seperate timeslots. 

we recurse over each 'row'. We check to see if the 4th cell contains the name of a lectu-
re or tutorial section, in which case we consolidate the current lecture or tutorial into
the session, and create a new lecture of tutorial.

there are five cases, the last of which is useless and only in place for pattern complet-
eness. 
  1. no more information, with last block being a tutorial. 
  2. no more information, with last block being a lecture.
  3. moer information with last block being a lecture
  4. more information with last block being a tutorial.
within 3. and 4, if the current row is 
----------------------------------------------------------------------------------------}
updateSession :: [[T.Text]] -> CourseSlot -> Maybe Tutorial -> Maybe Lecture -> Session -> Session
updateSession [] slot (Just tut) Nothing sesh = sesh {tutorials = tut:(tutorials sesh)}
updateSession [] slot Nothing (Just lec) sesh = sesh {lectures = lec:(lectures sesh)}
updateSession course slot (Just tut) Nothing sesh = 
  let row = head course
      updatedSlot = updateSlot row slot
  in  if (row !! 3) == ""
      then 
        updateSession (tail course)
                      updatedSlot 
                      (Just (makeTutorial updatedSlot (Just tut)))
                      Nothing
                      sesh
      else  if (T.head (row !! 3)) == 'L' --tutorial
            then
              updateSession (tail course)
                            updatedSlot
                            Nothing
                            (Just (makeLecture updatedSlot Nothing))
                            (sesh {tutorials = tut:(tutorials sesh)}) 
            else 
              updateSession (tail course)
                            updatedSlot
                            (Just (makeTutorial updatedSlot Nothing))
                            Nothing
                            (sesh {tutorials = tut:(tutorials sesh)})-- lecture
              
updateSession course slot Nothing (Just lec) sesh = 
  let row = head course
      updatedSlot = updateSlot row slot
  in  if (row !! 3) == ""
      then 
        updateSession (tail course)
                      updatedSlot 
                      Nothing
                      (Just (makeLecture updatedSlot (Just lec)))
                      sesh
      else  if (T.head (row !! 3)) == 'L' --Lecture
            then --lecture
              updateSession (tail course)
                            updatedSlot
                            Nothing
                            (Just (makeLecture updatedSlot Nothing))
                            sesh {lectures =  lec : (lectures sesh)}
              
            else --tutorial or practical
              updateSession (tail course)
                            updatedSlot
                            (Just (makeTutorial updatedSlot Nothing))
                            Nothing
                            sesh {lectures = lec:(lectures sesh)}
              
updateSession _ _  Nothing Nothing sesh = sesh


{----------------------------------------------------------------------------------------
it is given a 2-d list of 'cells' representing all information for a single course.
using updateSession, it converts this into a Session. Each lecture and tutorial within
the session is then inserted into their respective tables. 

QUIRK: re-checking for 'Cancelled' courses, catching case where the first lecture sect-
ion (same row that contains course name) is cancelled.
----------------------------------------------------------------------------------------}
processCourseTable :: [[T.Text]] -> IO ()
processCourseTable course = do 
  let  session = ((head course) !! 1)
  let  code = T.take 8 ((head course) !! 0)
  let  emptySesh = Session {lectures = [], tutorials = []}
  let  finalSlot = if (head course) !! 5  == "Cancel"
                   then updateSlot (head (tail course)) emptySlot
                   else updateSlot (head course) emptySlot
  let  sesh = if (head course) !! 5  == "Cancel"
              then updateSession (tail (tail course)) finalSlot Nothing (Just (makeLecture finalSlot Nothing)) emptySesh   
              else updateSession (tail course) finalSlot Nothing (Just (makeLecture finalSlot Nothing)) emptySesh   
  print code
  runSqlite dbStr $ do
    runMigration migrateAll
    mapM_ (\l ->  insertLec session code l) (lectures sesh)
    mapM_ (\t ->  insertTut session code t) (tutorials sesh)         

{----------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------}
main :: IO ()
main = do
    rsp <- simpleHTTP (getRequest $ timetableUrl ++ "sponsors.htm")
    body <- getResponseBody rsp
    let depts = getDeptList $ parseTags body
    --getDeptTimetable "online.html"
    mapM_ getDeptTimetable depts