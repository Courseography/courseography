{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module WebParsing.PostParser
    (getPost) where

import Network.HTTP
import Database.PostInsertion(insertPost, insertPostCategory)
import Database.Persist.Sqlite(runSqlite, runMigration)
import Config (databasePath)
import WebParsing.ParsingHelp
import qualified Data.Text as T
import Data.List
import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Tables
import qualified Text.Parsec as P
import WebParsing.ParsecCombinators(getCourseFromTag, getPostType, getDepartmentName,
    parsingAlgoOne)

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
        fullPostName = innerText (take 1 $ filter (isTagText) tags)
        postType = T.pack $ getPostType postCode
        departmentName = T.pack $ (getDepartmentName fullPostName postType)
        prereqs = map getCourseFromTag $ map (fromAttrib "href") $ filter isCourseTag tags
    addPostCategoriesToDatabase (T.unpack postCode) (innerText tags)
    insertPost departmentName postType postCode
    where
        isCourseTag tag = tagOpenAttrNameLit "a" "href" (\hrefValue -> (length hrefValue) >= 0) tag

addPostCategoriesToDatabase :: String -> String -> IO ()
addPostCategoriesToDatabase postCode tagText  = do
    let parsed = P.parse parsingAlgoOne "(source)" tagText
    case parsed of
        Right text ->
            mapM_ (addCategoryToDatabase postCode) (filter isCategory text)
        Left _ -> print "Failed."
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
