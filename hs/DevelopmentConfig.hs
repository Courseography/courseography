{-# LANGUAGE OverloadedStrings #-}

{-
Description: Holds constants that could change between the development and production environments.

To make the project compile, this file needs to be named Config.hs to match the module name.
When the project is deployed to the production environment, this file should be swapped with one
containing the production values.
-}

module Config
    (databasePath,
     markdownPath,
     graphPath,
     genCssPath,
     cssStyle,
     enableFb,
     firstMondayFall,
     lastWednesdayFall
     firstMondayWinter,
     lastMondayWinter,
     outDay) where

import Data.Text (Text)
import qualified Clay.Render as Clay
import Data.Time (Day, fromGregorian)

-- DATABASE CONNECTION STRINGS

-- | The path to the database file, relative to @hs/@.
databasePath :: Text
databasePath = "Database/database2015.sqlite3"

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

-- CALENDAR RESPONSE DATES

-- | First Monday of the Fall term.
firstMondayFall :: Day
firstMondayFall = fromGregorian 2015 09 14

-- | Last Wednesday of the Fall term.
lastWednesdayFall :: Day
lastWednesdayFall = fromGregorian 2015 12 02

-- | First Monday of the Winter term.
firstMondayWinter :: Day
firstMondayWinter = fromGregorian 2016 01 11

-- | Last Monday of the Winter term.
lastMondayWinter :: Day
lastMondayWinter = fromGregorian 2016 04 04

-- | Out of date day.
outDay :: Day
outDay = fromGregorian 2014 01 01

-- FACEBOOK CONFIGURATION

-- | Enable Facebook integration. Should only be true on the production server.
enableFb :: Bool
enableFb = False