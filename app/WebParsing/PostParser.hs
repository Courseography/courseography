{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module WebParsing.PostParser
    (getPost) where

import Network.HTTP
import Database.PostInsertion (insertPost, insertPostCategory)
import qualified Data.Text as T
import Data.List
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Text.Parsec as P
import WebParsing.ParsecCombinators (getCourseFromTag, parsingAlgoOne, generalCategoryParser, parseCategory,
    postInfoParser)

fasCalendarURL :: String
fasCalendarURL = "http://calendar.artsci.utoronto.ca/"

failedString :: String
failedString = "Failed."

getPost :: String -> IO ()
    getPost str = do
    let path = fasCalendarURL ++ str
    rsp <- simpleHTTP (getRequest path)
    body <- getResponseBody rsp
    let tags = filter isNotComment $ parseTags body
        postsSoup = secondH2 tags
        posts = partitions isPostName postsSoup
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

addPostToDatabase :: [Tag String] -> IO ()
addPostToDatabase tags = do
    let postCode = T.pack (fromAttrib "name" ((take 1 $ filter (isTagOpenName "a") tags) !! 0))
        liPartitions = partitions isLiTag tags
        prereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag tags
        firstCourse = if (null prereqs) then Nothing else (Just (head prereqs))
    case liPartitions of
        [] -> generalParser tags firstCourse postCode
        other -> liParser tags liPartitions firstCourse postCode
    where
        isCourseTag tag = tagOpenAttrNameLit "a" "href" (\hrefValue -> (length hrefValue) >= 0) tag
        isLiTag tag = isTagOpenName "li" tag

addPostCategoriesToDatabase :: String -> [String] -> IO ()
addPostCategoriesToDatabase postCode categories = do
    mapM_ (addCategoryToDatabase postCode) (filter isCategory categories)
    where
        isCategory string =
            let infixes = map (containsString string)
                         ["First", "Second", "Third", "suitable", "Core", "Electives"]
            in
                ((length string) >= 7) && ((length $ filter (\bool -> bool) infixes) <= 0)
        containsString string substring = isInfixOf substring string

addCategoryToDatabase :: String -> String -> IO ()
addCategoryToDatabase postCode category =
    insertPostCategory (T.pack category) (T.pack postCode)


-- Helpers

generalParser :: [Tag String] -> Maybe String -> T.Text -> IO ()
generalParser tags firstCourse postCode = do
    let parsed = P.parse (generalCategoryParser firstCourse) failedString (innerText tags)
    case parsed of
        Right (description, departmentName, postType, categories) -> do
            insertPost (T.pack departmentName) (T.pack postType) postCode (T.pack description)
            addPostCategoriesToDatabase (T.unpack postCode) categories
        Left message -> do
            print message

liParser :: [Tag String] -> [[Tag String]] -> Maybe String -> T.Text -> IO ()
liParser tags liPartitions firstCourse postCode = do
    let categories = map parseLi liPartitions
        postInfo = P.parse (postInfoParser firstCourse) failedString (innerText tags)
    case postInfo of
        Right (description, departmentName, postType) -> do
            insertPost (T.pack departmentName) (T.pack postType) postCode (T.pack description)
            addPostCategoriesToDatabase (T.unpack postCode) categories
        Left message -> do
            print message

parseLi :: [Tag String] -> String
parseLi liPartition = do
    let parsed = P.parse (parseCategory False) failedString (innerText liPartition)
    case parsed of
        Right category -> category
        Left message -> ""
