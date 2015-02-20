{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ArtSciParser (parseArtSci) where

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
import Database.Tables
import WebParsing.ParsingHelp
import Database.JsonParser
import Database.CourseQueries

fasCalendarURL :: String
fasCalendarURL = "http://www.artsandscience.utoronto.ca/ofr/calendar/"

{----------------------------------------------------------------------------------------
A collection of pages that do not contain any course information
----------------------------------------------------------------------------------------}
toDelete :: [String]
toDelete = ["199299398399Big_Ideas_(Faculty_of_Arts_&_Science_Programs).html",
            "Joint_Courses.html",
            "Writing_in_the_Faculty_of_Arts_&_Science.html",
            "crs_bio.htm",
            "Life_Sciences.html"]

{----------------------------------------------------------------------------------------
INPUT: 'processed' main page
OUTPUT: a list of department html page names
-----------------------------------------------------------------------------------------}
getDeptList :: [Tag String] -> [String]
getDeptList tags =
    let lists = sections (== TagOpen "ul" [("class","simple")]) tags
        contents = takeWhile (/= TagClose "ul") . head . tail $ lists
        as = filter (tagOpen (== "a") (\x -> True)) contents
        rawList = map (\x -> case x of TagOpen "a" [("href", link)] -> link) as
    in nub $ filter (\dept -> not (dept `elem` toDelete)) rawList

{----------------------------------------------------------------------------------------
INPUT: an html filename of a department (which are found from getDeptList)
OUTPUT: a list, where each element is a list of strings and tags relating to a single
course found in that department
----------------------------------------------------------------------------------------}
getCalendar :: String -> IO ()
getCalendar str = do
    let path = fasCalendarURL ++ str
    rsp <- simpleHTTP (getRequest path)
    body <- getResponseBody rsp
    let tags = filter isComment $ parseTags (T.pack body)
    let coursesSoup = lastH2 tags
    let courses = map (filter (tagText (\x -> True))) $ partitions isCourseTitle coursesSoup
    let course = map processCourseToData courses
    runSqlite dbStr $ do
      runMigration migrateAll
      mapM_ insertCourse course
    where
        isComment (TagComment _) = False
        isComment _ = True
        lastH2 = last . sections (tagOpen (== "h2") (\x -> True))
        isCourseTitle (TagOpen _ attrs) = any (\x -> fst x == "name" && T.length (snd x) == 8) attrs
        isCourseTitle _ = False

parseTitleFAS :: CoursePart -> CoursePart
parseTitleFAS (tags, course) =
    let courseNames = T.splitAt 8 $ removeTitleGarbage $ removeLectureSection $ head tags
  in (tail tags, course {title = Just (snd courseNames),
                         name =  fst courseNames})
  where removeLectureSection (TagText s) = T.takeWhile (/= '[') s
        removeTitleGarbage s = replaceAll ["\160"] "" s
{----------------------------------------------------------------------------------------
INPUT: a list of tags representing a single course,
OUTPUT: Course 'record' containing course info
----------------------------------------------------------------------------------------}
processCourseToData :: [Tag T.Text] -> Course
processCourseToData tags  =
    let course =
          Course {
            breadth = Nothing,
            description = Nothing,
            title  = Nothing,
            prereqString = Nothing,
            f = Nothing,
            s = Nothing,
            y = Nothing,
            name = T.empty,
            exclusions = Nothing,
            manualTutorialEnrol = Nothing,
            distribution = Nothing,
            prereqs = Nothing
        }
    in snd $ (tags, course) ~:
             preProcess -:
             parseTitleFAS -:
             parseDescription -:
             parsePrerequisite -:
             parseCorequisite -:
             parseExclusion -:
             parseRecommendedPrep -:
             parseDistAndBreadth

parseArtSci :: IO ()
parseArtSci = do
    rsp <- simpleHTTP (getRequest fasCalendarURL)
    body <- getResponseBody rsp
    let depts = getDeptList $ parseTags  body
    putStrLn "Parsing Arts and Science Calendar..."
    mapM_ getCalendar depts


main :: IO ()
main = do
    rsp <- simpleHTTP (getRequest fasCalendarURL)
    body <- getResponseBody rsp
    let depts = getDeptList $ parseTags  body
    mapM_ getCalendar depts

