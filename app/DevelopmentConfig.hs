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
    fasCalendarUrl,
    programsUrl,
    createReqBody,
    reqHeaders,
    fallStartDate,
    fallEndDate,
    winterStartDate,
    winterEndDate,
    outDay,
    holidays
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, fromGregorian)
import Happstack.Server (Conf (..), LogAccess, nullConf)
import System.Log.Logger (Priority (INFO), logM)
import Network.HTTP.Types.Header (RequestHeaders)

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
timetableUrl = "https://ttb.utoronto.ca/"

-- | The Faculty of Arts and Science API for course timetables (by unit).
timetableApiUrl :: Text
timetableApiUrl = "https://api.easi.utoronto.ca/ttb/getPageableCourses"

-- | The URLs of the Faculty of Arts & Science calendar.
fasCalendarUrl :: String
fasCalendarUrl = "https://artsci.calendar.utoronto.ca/"

programsUrl :: String
programsUrl = "https://artsci.calendar.utoronto.ca/listing-program-subject-areas"

-- HTTP REQUEST STRINGS

-- | Create the body for the HTTP request based on the org
createReqBody :: Text -> Value
createReqBody org = object [ "campuses" .= ([] :: [T.Text]),
                       "courseCodeAndTitleProps" .= object
                       [ "courseCode" .= ("" :: T.Text),
                         "courseSectionCode" .= ("" :: T.Text),
                         "courseTitle" .= org,
                         "searchCourseDescription" .= True
                       ],
                        "courseLevels" .= ([] :: [T.Text]),
                        "creditWeights" .= ([] :: [T.Text]),
                        "dayPreferences" .= ([] :: [T.Text]),
                        "deliveryModes" .= ([] :: [T.Text]),
                        "departmentProps" .= ([] :: [T.Text]),
                        "direction" .= ("asc" :: T.Text),
                        "divisions" .= [T.pack "ARTSC"],
                        "instructor" .= ("" :: T.Text),
                        "page" .= (1 :: Int),
                        "pageSize" .= (200 :: Int),
                        "requirementProps" .= ([] :: [T.Text]),
                        "sessions" .= [T.pack "20249", T.pack "20251", T.pack "20249-20251"],
                        "timePreferences" .= ([] :: [T.Text])
                     ]

-- | The headers for the HTTP request
reqHeaders :: RequestHeaders
reqHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")]

-- CALENDAR RESPONSE DATES

-- | First day of classes for the fall term.
fallStartDate :: Day
fallStartDate = fromGregorian 2024 09 03

-- | Last day of classes for the fall term.
fallEndDate :: Day
fallEndDate = fromGregorian 2024 12 03

-- | First day of classes for the winter term.
winterStartDate :: Day
winterStartDate = fromGregorian 2025 01 06

-- | Last day of classes for the winter term.
winterEndDate :: Day
winterEndDate = fromGregorian 2025 04 04

-- | Out of date day. Used to control forbidden inputs for days.
outDay :: Day
outDay = fromGregorian 2026 01 01

-- Holidays for the fall and winter term.
holidays :: [String]
holidays = ["20241014T", "20241028T", "20241029T",
            "20241030T", "20241031T", "20241101T",
            "20250217T", "20250218T", "20240219T",
            "20250220T", "20250221T"]
