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
    cssStyle,
    fallStartDate,
    fallEndDate,
    winterStartDate,
    winterEndDate,
    outDay,
    holidays,
    enableCdn
    ) where

import Data.Text (Text)
import qualified Clay.Render as Clay
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

-- | The path to the database file, relative to @hs/@.
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
timetableUrl = "http://www.artsandscience.utoronto.ca/ofr/timetable/winter/"

-- | The Faculty of Arts and Science API for course timetables (by unit).
timetableApiUrl :: Text
timetableApiUrl = "https://timetable.iit.artsci.utoronto.ca/api/20199/courses?org="

-- | The Faculty of Arts and Science API for a list of all units.
orgApiUrl :: String
orgApiUrl = "https://timetable.iit.artsci.utoronto.ca/api/orgs"

-- | The URLs of the Faculty of Arts & Science calendar.
fasCalendarUrl :: String
fasCalendarUrl = "https://fas.calendar.utoronto.ca/"

programsUrl :: String
programsUrl = "https://fas.calendar.utoronto.ca/listing-program-subject-areas"

-- ASSET COMPILATION

-- | Output css style. Either @Clay.pretty@ for human-readable output or
-- @Clay.compact@ for minified output.
cssStyle :: Clay.Config
cssStyle = Clay.pretty

-- CALENDAR RESPONSE DATES

-- | First day of classes for the fall term.
fallStartDate :: Day
fallStartDate = fromGregorian 2019 09 05

-- | Last day of classes for the fall term.
fallEndDate :: Day
fallEndDate = fromGregorian 2019 12 04

-- | First day of classes for the winter term.
winterStartDate :: Day
winterStartDate = fromGregorian 2020 01 06

-- | Last day of classes for the winter term.
winterEndDate :: Day
winterEndDate = fromGregorian 2020 04 03

-- | Out of date day. Used to control forbidden inputs for days.
outDay :: Day
outDay = fromGregorian 2019 01 01

-- Holidays for the fall and winter term.
holidays :: [String]
holidays = ["20191014T", "20191104T", "20191105T",
            "20191106T", "20191107T", "20191108T",
            "20200217T", "20200218T", "20200219T",
            "20200220T", "20200221T"]

-- SCRIPT DEPENDENCIES CONFIGURATION

-- | Enable CDN downloads for js and css dependencies. Should be true on the production server.
enableCdn :: Bool
enableCdn = True
