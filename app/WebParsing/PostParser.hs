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
import qualified Text.Parsec as P
import Text.Parsec ((<|>))

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
    insertPost departmentName postType postCode

getPostType :: T.Text -> String
getPostType postCode = 
    let codeSection = extractPostType (T.unpack postCode)
    in
        case codeSection of
            "SPE" -> "Specialist"
            "MAJ" -> "Major"
            "MIN" ->  "Minor"

extractPostType :: [Char] -> [Char]
extractPostType postCode = do
    let parsed = P.parse findPostType "(source)" postCode
    case parsed of 
        Right name -> name
        Left _ -> ""

findPostType :: P.Parsec String () String
findPostType = do
   P.string "AS"
   P.many1 P.letter

getDepartmentName :: [Char] -> T.Text -> [Char]
getDepartmentName fullPostName postType = do
    let parsed = P.parse (isDepartmentName (T.unpack postType)) "(source)" fullPostName
    case parsed of 
        Right name -> name
        Left _ -> ""

isDepartmentName ::  [Char] -> P.Parsec String () String
isDepartmentName postType = P.manyTill P.anyChar (P.try (P.string postType))
