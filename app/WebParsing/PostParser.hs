{-# LANGUAGE OverloadedStrings #-}
module WebParsing.PostParser
    (parsePosts) where

import Database.PostInsertion(insertPost, insertPostCategory)

parsePosts :: IO ()
parsePosts = do
    putStrLn "Parsing Posts..."
    insertPost "Computer Science" "Major" "AJSM24"
    insertPostCategory "CSC148" "Computer Science"