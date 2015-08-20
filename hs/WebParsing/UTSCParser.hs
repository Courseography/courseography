{-# LANGUAGE OverloadedStrings #-}
module WebParsing.UTSCParser
    (parseUTSC) where

import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Persist.Sqlite
import qualified Data.Text as T
import Database.Tables as Tables
import WebParsing.ParsingHelp

parseUTSC :: IO ()
parseUTSC = do
    rsp <- simpleHTTP (getRequest (utscCalendarUrl ++ "Table_of_Contents.html"))
    body <- getResponseBody rsp
    let depts = getDeptList $ parseTags body
    putStrLn "Parsing UTSC Calendar..."
    mapM_ getCalendar depts

utscCalendarUrl :: String
utscCalendarUrl = "http://www.utsc.utoronto.ca/~registrar/calendars/calendar/"

getDeptList :: [Tag String] -> [String]
getDeptList tags =
    let beforeList = dropWhile (/= TagText "\nPrograms and Courses\n") tags
        removeUls = dropBetweenAll (== TagOpen "ul" [("class", "circle")]) (== TagClose "ul") beforeList
        hrefs = filter (tagOpen (== "a") isHref) removeUls
    in takeWhile (/= "Admissions.html") $ map getAttribute hrefs
    where
        isHref [("href", _)] = True
        isHref _ = False
        getAttribute (TagOpen _ [(_, link)]) = link

getCalendar :: String -> IO ()
getCalendar str = do
    rsp <- simpleHTTP (getRequest (utscCalendarUrl ++ str))
    body <- getResponseBody rsp
    let tags = filter isntComment $ parseTags (T.pack body)
        coursesSoup =  takeWhile (/= TagOpen "div" [("id", "pdf_files")]) $ lastH2 tags
        courses = map (filter (tagText (const True))) $ partitions isCourseTitle coursesSoup
        course = map processCourseToData courses
    print ("parsing: " ++ str)
    runSqlite databasePath $ do
        runMigration migrateAll
        mapM_ insertCourse course
    where
        isntComment (TagComment _) = False
        isntComment _ = True
        lastH2 = last . sections (tagOpen (== "h2") (const True))
        isCourseTitle (TagOpen _ attrs) = any (\x -> fst x == "name" && T.length (snd x) == 8) attrs
        isCourseTitle _ = False

parseTitleUTSC :: CoursePart -> CoursePart
parseTitleUTSC (tags, course) =
    let nme = fromTagText $tags !! 0
        ttle = fromTagText $ tags !! 1
    in (drop 2 tags, course {name = nme, title = Just ttle})

{----------------------------------------------------------------------------------------
INPUT: a list of tags representing a single course,
OUTPUT: Course 'record' containing course info
----------------------------------------------------------------------------------------}
processCourseToData :: [Tag T.Text] -> Course
processCourseToData tags  =
    let course = emptyCourse
    in snd $ (tags, course) ~:
             preProcess -:
             parseTitleUTSC -:
             parseDescription -:
             parsePrerequisite -:
             parseCorequisite -:
             parseExclusion -:
             parseRecommendedPrep -:
             parseDistAndBreadth
