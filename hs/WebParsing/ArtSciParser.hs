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


-- |A collection of pages that do not contain any course information
toDelete :: [String]
toDelete = ["199299398399Big_Ideas_(Faculty_of_Arts_&_Science_Programs).html",
            "Joint_Courses.html",
            "Writing_in_the_Faculty_of_Arts_&_Science.html",
            "crs_bio.htm",
            "Life_Sciences.html"]


-- | converts the processed main page and extracts a list of department html pages
getDeptList :: [Tag String] -> [String]
getDeptList tags =
    let lists = sections (tagOpenAttrNameLit "ul" "class" (=="simple")) tags
        contents = takeWhile (not. isTagCloseName "ul") . head . tail $ lists
        as = filter (isTagOpenName "a") contents
        rawList = nub $ map (fromAttrib "href") as
    in rawList \\ toDelete


-- | Takes an html filename of a department (which are found from getDeptList) and returns
--  a list, where each element is a list of strings and tags relating to a single
-- course found in that department.
getCalendar :: String -> IO ()
getCalendar str = do
    let path = fasCalendarURL ++ str
    rsp <- simpleHTTP (getRequest path)
    body <- getResponseBody rsp
    let tags = filter isNotComment $ parseTags (T.pack body)
    let coursesSoup = lastH2 tags
    let course = map (processCourseToData . (filter isTagText)) $ partitions isCourseTitle coursesSoup
    print $ "parsing " ++ str
    mapM_ (\x -> print (prereqs x)) course 
    runSqlite dbStr $ do
        runMigration migrateAll
        mapM_ insertCourse course
    where
        isNotComment (TagComment _) = False
        isNotComment _ = True
        lastH2 = last . sections (isTagOpenName "h2")
        isCourseTitle (TagOpen _ attrs) = any (\x -> fst x == "name" && T.length (snd x) == 8) attrs
        isCourseTitle _ = False


parseTitleFAS :: CoursePart -> CoursePart
parseTitleFAS (tag:tags, course) =
    let (n, t) = T.splitAt 8 $ removeTitleGarbage $ removeLectureSection tag
    in (tags, course {title = Just t, name =  n})
    where removeLectureSection (TagText s) = T.takeWhile (/= '[') s
          removeTitleGarbage s = replaceAll ["\160"] "" s

-- |takes a list of tags representing a single course, and returns a course Record 
processCourseToData :: [Tag T.Text] ->  Course
processCourseToData tags  =
    let course = emptyCourse
    in  snd $ (tags, course) ~:
              preProcess -: 
              parseTitleFAS -:
              parseDescription -:
              parsePrerequisite -:
              parseCorequisite -:
              parseExclusion -:
              parseRecommendedPrep -:
              parseDistAndBreadth
-- | parses the entire Arts & Science Course Calendar and inserts courses
-- into the database.
parseArtSci :: IO ()
parseArtSci = do
    rsp <- simpleHTTP (getRequest fasCalendarURL)
    body <- getResponseBody rsp
    let depts = getDeptList $ parseTags body
    putStrLn "Parsing Arts and Science Calendar..."
    mapM_ getCalendar depts
