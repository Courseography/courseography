{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ArtSciParser
    (parseArtSci, getDeptList, fasCalendarURL) where

import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Persist.Sqlite
import Database.Persist (insertUnique)
import Database.CourseInsertion
import Data.List
import qualified Data.Text as T
import Database.Tables
import WebParsing.ParsingHelp
import Config (databasePath)
import WebParsing.PostParser
import Data.Maybe (mapMaybe)

fasCalendarURL :: String
fasCalendarURL = "http://calendar.artsci.utoronto.ca/"

-- | A collection of pages that do not contain any course information
toDelete :: [T.Text]
toDelete = ["199299398399(Faculty_of_Arts_&_Science_Programs).html",
            "Joint_Courses.html",
            "Writing_in_the_Faculty_of_Arts_&_Science.html",
            "crs_bio.htm",
            "Life_Sciences.html"]

-- | Converts the processed main page and extracts a list of department html pages
-- and department names
getDeptList :: [Tag T.Text] -> ([T.Text], [T.Text])
getDeptList tags =
    let lists = sections (tagOpenAttrNameLit "ul" "class" (== "simple")) tags
        contents = takeWhile (not. isTagCloseName "ul") . head . tail $ lists
        as = filter (isTagOpenName "a") contents
        rawList = nub $ map (fromAttrib "href") as
        ts = mapMaybe maybeTagText $ filter isTagText contents
        deptNames = filter (not . T.null) $ map T.strip ts
    in (rawList \\ toDelete, deptNames)

-- | Takes an html filename of a department (which are found from getDeptList) and returns
-- a list, where each element is a list of strings and tags relating to a single
-- course found in that department.
getCalendar :: T.Text -> IO ()
getCalendar htmlText = do
    let htmlStr = T.unpack htmlText
        path = fasCalendarURL ++ htmlStr
    rsp <- simpleHTTP (getRequest path)
    body <- getResponseBody rsp
    let tags = filter isNotComment $ parseTags (T.pack body)
        coursesSoup = lastH2 tags
        course = map (processCourseToData . filter isTagText) $ partitions isCourseTitle coursesSoup
    print $ "parsing " ++ htmlStr
    runSqlite databasePath $ do
        runMigration migrateAll
        mapM_ insertCourse course
    where
        isNotComment (TagComment _) = False
        isNotComment _ = True
        lastH2 tags =
            let sect = sections (isTagOpenName "h2") tags
            in
                if null sect
                then
                    []
                else
                    last sect
        isCourseTitle (TagOpen _ attrs) = any (\x -> fst x == "name" && T.length (snd x) == 8) attrs
        isCourseTitle _ = False

parseTitleFAS :: CoursePart -> CoursePart
parseTitleFAS (tag:tags, course) =
    let (n, t) = T.splitAt 8 $ removeTitleGarbage $ removeLectureSection tag
    in (tags, course {title = Just t, name =  n})
    where removeLectureSection (TagText str) = T.takeWhile (/= '[') str
          removeTitleGarbage = replaceAll ["\160"] ""

-- | Takes a list of tags representing a single course, and returns a course Record
processCourseToData :: [Tag T.Text] ->  Course
processCourseToData tags  =
    let course = emptyCourse
    in snd $ (tags, course) ~:
             preProcess -:
             parseTitleFAS -:
             parseDescription -:
             parsePrerequisite -:
             parseCorequisite -:
             parseExclusion -:
             parseRecommendedPrep -:
             parseDistAndBreadth

-- | Insert deparment names to database
getDepts :: [T.Text] -> IO ()
getDepts depts = runSqlite databasePath $
    (mapM_ (insertUnique . Department) depts :: SqlPersistM ())

-- | Parses the entire Arts & Science Course Calendar and inserts courses
-- into the database.
parseArtSci :: IO ()
parseArtSci = do
    rsp <- simpleHTTP (getRequest fasCalendarURL)
    body <- (getResponseBody rsp)
    let (depts, deptNames) = getDeptList $ parseTags (T.pack body)
    putStrLn "Inserting departments"
    getDepts deptNames
    putStrLn "Parsing Arts and Science Posts"
    mapM_ getPost depts
    putStrLn "Parsing Arts and Science Calendar..."
    mapM_ getCalendar depts
