{-# LANGUAGE OverloadedStrings #-}
module WebParsing.PostParser
    (getPost) where

import Network.HTTP
import Database.PostInsertion(insertPost, insertPostCategory)
import Database.Persist.Sqlite(runSqlite, runMigration)
import Config (databasePath)
import WebParsing.ParsingHelp
import qualified Data.Text as T
import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Tables

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
addPostToDatabase tags =
    let postCode = T.pack (fromAttrib "name" ((take 1 $ filter (isTagOpenName "a") tags) !! 0))
        fullPostName = innerText (take 1 $ filter (isTagText) tags)
        postType =
            -- Note: See Biochemistry Specialist calendar entry
            if length (words fullPostName) > 2
            then T.pack ((reverse $ words $ fullPostName) !! 2)
            else ""
        departmentName = T.pack $ unwords $ reverse $ drop 3  $ reverse $ words $ fullPostName
    in
        insertPost departmentName postType postCode
