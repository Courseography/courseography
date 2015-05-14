{-# LANGUAGE OverloadedStrings #-}

module Config (dbStr, fbdbStr) where

import Data.Text (Text)

-- DATABASE CONNECTION STRINGS

dbStr :: Text
dbStr = "Database/database2015.sqlite3"

fbdbStr :: Text
fbdbStr = "fdatabase1.sqlite3"
