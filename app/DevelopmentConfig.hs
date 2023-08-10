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
                        "sessions" .= [T.pack "20239", T.pack "20241", T.pack "20239-20241"],
                        "timePreferences" .= ([] :: [T.Text])
                     ]

-- | The headers for the HTTP request
reqHeaders :: RequestHeaders
reqHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")]

-- CALENDAR RESPONSE DATES

-- | First day of classes for the fall term.
fallStartDate :: Day
fallStartDate = fromGregorian 2023 09 07

-- | Last day of classes for the fall term.
fallEndDate :: Day
fallEndDate = fromGregorian 2023 12 06

-- | First day of classes for the winter term.
winterStartDate :: Day
winterStartDate = fromGregorian 2024 01 08

-- | Last day of classes for the winter term.
winterEndDate :: Day
winterEndDate = fromGregorian 2024 04 05

-- | Out of date day. Used to control forbidden inputs for days.
outDay :: Day
outDay = fromGregorian 2024 01 01

-- Holidays for the fall and winter term.
holidays :: [String]
holidays = ["20231009T", "20231106T", "20231107T",
            "20231108T", "20231109T", "20231110T",
            "20240219T", "20240220T", "20230221T",
            "20240222T", "20240223T"]
