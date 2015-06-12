{-# LANGUAGE OverloadedStrings #-}

{-
Description: Holds constants that could change between the development and production environments.

To make the project compile, this file needs to be named Config.hs to match the module name.
When the project is deployed to the production environment, this file should be swapped with one
containing the production values.
-}

module Config (dbStr,
               markdownPath,
               graphPath,
               genCssPath,
               cssStyle) where

import Data.Text (Text)
import qualified Clay.Render as Clay
import Data.Time

-- DATABASE CONNECTION STRINGS

-- | The path to the database file, relative to @hs/@.
dbStr :: Text
dbStr = "Database/database2015.sqlite3"

-- FILE PATH STRINGS

-- | The relative path to the directory with the markdown files rendered for site content.
markdownPath :: String
markdownPath = "../"

-- | The relative path to the directory that contains all of the graph SVG files.
graphPath :: String
graphPath = "../public/res/graphs/"

-- | The relative path to the directory containing all of the generated CSS files.
genCssPath :: String
genCssPath = "../public/style/"

-- ASSET COMPILATION

-- | Output css style. Either @Clay.pretty@ for human-readable output or
-- @Clay.compact@ for minified output.
cssStyle :: Clay.Config
cssStyle = Clay.pretty

firstMondayFall :: Day
firstMondayFall = fromGregorian 2015 09 14

firstMondayWinter :: Day
firstMondayWinter = fromGregorian 2016 01 11