{-# LANGUAGE OverloadedStrings #-}
module WebParsing.PostParser
    (parsePosts) where

import Network.HTTP
import Database.PostInsertion(insertPost, insertPostCategory)
import Database.Persist.Sqlite
import WebParsing.ArtSciParser(getDeptList)
import WebParsing.ParsingHelp
import qualified Data.Text as T
import Data.Char
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

fasCalendarURL :: String
fasCalendarURL = "http://calendar.artsci.utoronto.ca/"

getPost :: String -> IO ()
getPost str = do
    let path = fasCalendarURL ++ str
    rsp <- simpleHTTP (getRequest path)
    body <- getResponseBody rsp
    let tags = filter isNotComment $ parseTags (T.pack body)
        programs = secondH2 tags
    print $ "parsing " ++ str
    print programs

    where 
        isNotComment (TagComment _) = False
        isNotComment _ = True
        secondH2 tags =
            let sect = sections (isTagOpenName "h2") tags
            in
                if null sect
                then
                    []
                else
                    sect !! 1

-- | Parses the entire Arts & Science Course Calendar and inserts posts
-- into the database.
parsePosts :: IO ()
parsePosts = do
    rsp <- simpleHTTP (getRequest fasCalendarURL)
    body <- getResponseBody rsp
    let depts = getDeptList $ parseTags body

    putStrLn "Parsing Posts..."
    mapM_ getPost depts
