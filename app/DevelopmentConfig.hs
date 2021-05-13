{-
Description: Holds constants that could change between the development and production environments.

To make the project compile, this file needs to be named Config.hs to match the module name.
When the project is deployed to the production environment, this file should be swapped with one
containing the production values.
-}

module Config (
    serverConf,
    databasePath,
    markdownPath,
    graphPath,
    genCssPath,
    timetableUrl,
    timetableApiUrl,
    orgApiUrl,
    fasCalendarUrl,
    programsUrl,
    fallStartDate,
    fallEndDate,
    winterStartDate,
    winterEndDate,
    outDay,
    holidays,
    enableCdn
    ) where

import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Happstack.Server (Conf(..), LogAccess, nullConf)
import System.Log.Logger (logM, Priority(INFO))

-- SERVER CONFIGURATION

-- | Server configuration settings.
serverConf :: Conf
serverConf = nullConf {
        port      = 8000,
        logAccess = Just logMAccessShort
    }

-- | Server log configuration. Default is to log access requests using hslogger
-- and a condensed log formatting.
logMAccessShort :: LogAccess t
logMAccessShort host user _ requestLine responseCode _ referer _ =
    logM "Happstack.Server.AccessLog.Combined" INFO $ unwords [
        host,
        user,
        requestLine,
        show responseCode,
        referer
        ]


-- DATABASE CONNECTION STRINGS

-- | The path to the database file, relative to the project root.
databasePath :: Text
databasePath = "db/database.sqlite3"

-- FILE PATH STRINGS

-- | The relative path to the directory with the markdown files rendered for site content.
markdownPath :: String
markdownPath = "./"

-- | The relative path to the directory that contains all of the graph SVG files.
graphPath :: String
graphPath = "./graphs/"

-- | The relative path to the directory containing all of the generated CSS files.
genCssPath :: String
genCssPath = "./public/style/"

-- URLs

-- | The URL for U of T's official timetable.
timetableUrl :: String
timetableUrl = "https://timetable.iit.artsci.utoronto.ca/"

-- | The Faculty of Arts and Science API for course timetables (by unit).
timetableApiUrl :: Text
timetableApiUrl = "https://timetable.iit.artsci.utoronto.ca/api/20209/courses?org="

-- | The Faculty of Arts and Science API for a list of all units.
orgApiUrl :: String
orgApiUrl = "https://timetable.iit.artsci.utoronto.ca/api/orgs"

-- | The URLs of the Faculty of Arts & Science calendar.
fasCalendarUrl :: String
fasCalendarUrl = "https://artsci.calendar.utoronto.ca/"

programsUrl :: String
programsUrl = "https://artsci.calendar.utoronto.ca/listing-program-subject-areas"

-- CALENDAR RESPONSE DATES

-- | First day of classes for the fall term.
fallStartDate :: Day
fallStartDate = fromGregorian 2019 09 10

-- | Last day of classes for the fall term.
fallEndDate :: Day
fallEndDate = fromGregorian 2019 12 09

-- | First day of classes for the winter term.
winterStartDate :: Day
winterStartDate = fromGregorian 2020 01 04

-- | Last day of classes for the winter term.
winterEndDate :: Day
winterEndDate = fromGregorian 2020 04 01

-- | Out of date day. Used to control forbidden inputs for days.
outDay :: Day
outDay = fromGregorian 2020 01 01

-- Holidays for the fall and winter term.
holidays :: [String]
holidays = ["20191012T", "20191109T", "20191110T",
            "20191111T", "20191112T", "20191113T",
            "20200215T", "20200216T", "20200217T",
            "20200218T", "20200219T"]

-- SCRIPT DEPENDENCIES CONFIGURATION

-- | Enable CDN downloads for js and css dependencies. Should be true on the production server.
enableCdn :: Bool
enableCdn = True
