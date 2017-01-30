{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module WebParsing.PostParser
    (getPost) where

import Network.HTTP
import qualified Data.Text as T
import Data.List
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Config (databasePath)
import Database.Tables
import Database.Persist.Sqlite (insert_, runMigration, runSqlite, SqlPersistM)
import qualified Text.Parsec as P
import WebParsing.ParsecCombinators (getCourseFromTag, generalCategoryParser, parseCategory, 
    postInfoParser)

fasCalendarURL :: String
fasCalendarURL = "http://calendar.artsci.utoronto.ca/"

getPost :: String -> IO ()
getPost str = do
    let path = fasCalendarURL ++ str
    rsp <- simpleHTTP (getRequest path)
    body <- getResponseBody rsp
    let tags = filter isNotComment $ parseTags body
        postsSoup = secondH2 tags
        posts = partitions isPostName postsSoup
    runSqlite databasePath $ do 
        runMigration migrateAll
        mapM_ addPostToDatabase posts
    print $ "parsing " ++ str
    where
        isNotComment (TagComment _) = False
        isNotComment _ = True
        secondH2 tags =
            let sect = sections (isTagOpenName "h2") tags
            in
                if (length sect) < 2
                then
                    []
                else
                    takeWhile isNotCoursesSection tags
        isNotCoursesSection tag = not (tagOpenAttrLit "a" ("name", "courses") tag)
        isPostName tag = tagOpenAttrNameLit "a" "name" (\nameValue -> (length nameValue) == 9) tag

addPostToDatabase :: [Tag String] -> SqlPersistM ()
addPostToDatabase tags = do
    let postCode = T.pack (fromAttrib "name" ((take 1 $ filter (isTagOpenName "a") tags) !! 0))
        liPartitions = partitions isLiTag tags
        prereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag tags
        firstCourse = if (null prereqs) then Nothing else (Just (head prereqs))
    categoryParser tags firstCourse postCode liPartitions
    where
        isCourseTag tag = tagOpenAttrNameLit "a" "href" (\hrefValue -> (length hrefValue) >= 0) tag
        isLiTag tag = isTagOpenName "li" tag

addPostCategoriesToDatabase :: String -> [String] -> SqlPersistM ()
addPostCategoriesToDatabase postCode categories = do
    mapM_ (addCategoryToDatabase postCode) (filter isCategory categories)
    where
        isCategory string =
            let infixes = map (containsString string)
                         ["First", "Second", "Third", "suitable", "Core", "Electives"]
            in
                ((length string) >= 7) && ((length $ filter (\bool -> bool) infixes) <= 0)
        containsString string substring = isInfixOf substring string

addCategoryToDatabase :: String -> String -> SqlPersistM ()
addCategoryToDatabase postCode category =
    insert_ $ PostCategory (T.pack category) (T.pack postCode)


-- Helpers

categoryParser :: [Tag String] -> Maybe String -> T.Text -> [[Tag String]] -> SqlPersistM ()
categoryParser tags firstCourse postCode liPartitions = do
    case parsed of
        Right (description, departmentName, postType, categories) -> do
            addPostCategoriesToDatabase (T.unpack postCode) categories
            insert_ $ Post (T.pack postType) (T.pack departmentName) postCode (T.pack description)
        Left message -> do
            return ()
    where
        parsed = case liPartitions of 
            [] -> P.parse (generalCategoryParser firstCourse) "Failed." (innerText tags)
            partitions -> do
                let categories = map parseLi partitions
                (description, departmentName, postType) <- P.parse (postInfoParser firstCourse) "Failed." (innerText tags)
                return (description, departmentName, postType, categories)

parseLi :: [Tag String] -> String
parseLi liPartition = do
    let parsed = P.parse parseCategory "Failed." (innerText liPartition)
    case parsed of 
        Right category -> category
        Left message -> ""
