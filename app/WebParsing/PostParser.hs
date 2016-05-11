{-# LANGUAGE OverloadedStrings #-}
module WebParsing.PostParser
    (parsePosts) where

import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Database.Persist.Sqlite
import Database.PostInsertion(insertPost, insertPostCategory)
import Data.List
import qualified Data.Text as T
import Database.Tables
import WebParsing.ParsingHelp
import Config (databasePath)

parsePosts :: IO ()
parsePosts = do
    putStrLn "Parsing Posts..."
    insertPost "Computer Science" "Major" "AJSM24"
    insertPostCategory "CSC148" "Computer Science"