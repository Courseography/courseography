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
     timetableUrl,
     cssStyle,
     enableFb,
     firstMondayFall,
     lastWednesdayFall,
     firstMondayWinter,
     lastMondayWinter,
     outDay,
     holidays) where

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

-- URLs

-- | The URL for U of T's official timetable.
timetableUrl :: String
timetableUrl = "http://www.artsandscience.utoronto.ca/ofr/timetable/winter/"

-- ASSET COMPILATION

-- | Output css style. Either @Clay.pretty@ for human-readable output or
-- @Clay.compact@ for minified output.
cssStyle :: Clay.Config
cssStyle = Clay.pretty

-- CALENDAR RESPONSE DATES

-- | First day of classes for the fall term. Also the first day to be 
-- assigned for a course scheduled on a Monday.
firstMondayFall :: Day
firstMondayFall = fromGregorian 2015 09 14

-- | Last Wednesday of the fall term. Used to generate the last
-- event for all courses that take place during the fall.
lastWednesdayFall :: Day
lastWednesdayFall = fromGregorian 2015 12 02

-- | First day of classes for the winter term. Also the first day to be 
-- assigned for a course scheduled on a Wednesday.
firstMondayWinter :: Day
firstMondayWinter = fromGregorian 2016 01 11

-- | Last Monday of the winter term. Used to generate the last
-- event for all courses that take place during the winter.
lastMondayWinter :: Day
lastMondayWinter = fromGregorian 2016 04 04

-- | Out of date day. Used to control forbidden inputs for days.
outDay :: Day
outDay = fromGregorian 2014 01 01

-- Holidays for the fall and winter term 2015/2016.
holidays :: [String]
holidays = ["20151012T", "20151109T", "20151110T",
            "20160215T", "20160216T", "20160217T",
            "20160218T", "20160219T", "20160325T"]

-- FACEBOOK CONFIGURATION

-- | Enable Facebook integration. Should only be true on the production server.
enableFb :: Bool
enableFb = False
