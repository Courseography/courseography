{-# LANGUAGE OverloadedStrings #-}

{-
Description: Holds constants that could change between the development and production environments.

To make the project compile, this file needs to be named Config.hs to match the module name.
When the project is deployed to the production environment, this file should be swapped with one
containing the production values.
-}

module Config (dbStr) where

import Data.Text (Text)

-- DATABASE CONNECTION STRINGS

-- | The path to the database file, relative to @hs/@.
dbStr :: Text
dbStr = "Database/database2015.sqlite3"

