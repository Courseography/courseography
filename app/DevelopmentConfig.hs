{-
Description: Holds constants that could change between the development and production environments.

To make the project compile, this file needs to be named Config.hs to match the module name.
When the project is deployed to the production environment, this file should be swapped with one
containing the production values.
-}

module Config (
    loadConfig,
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

import Data.Yaml.Config (loadYamlSettings, useEnv)
import Data.Aeson (FromJSON(..), object, (.=), Value, (.:), withObject)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time (Day)
import Happstack.Server (Conf (..), LogAccess, nullConf)
import Network.HTTP.Types.Header (RequestHeaders, HeaderName)
import System.Log.Logger (Priority (INFO), logM)
import qualified Data.CaseInsensitive as CI
import Data.ByteString.Char8 (pack)

-- Main configuration data type
data Config = Config
    { portValue             :: Int
    , logMessage            :: String
    , databasePathValue     :: Text
    , markdownPathValue     :: String
    , graphPathValue        :: String
    , genCssPathValue       :: String
    , timetableUrlValue     :: String
    , timetableApiUrlValue  :: Text
    , fasCalendarUrlValue   :: String
    , programsUrlValue      :: String
    , directionValue        :: T.Text
    , divisionsValue        :: [T.Text]
    , pageValue             :: Int
    , pageSizeValue         :: Int
    , sessionsValue         :: [T.Text]
    , reqHeadersValue       :: RequestHeaders
    , fallStartDateValue    :: Day
    , fallEndDateValue      :: Day
    , winterStartDateValue  :: Day
    , winterEndDateValue    :: Day
    , outDayValue           :: Day
    , holidaysList          :: [String]
    } deriving (Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \obj -> Config
        <$> obj .: "port"
        <*> obj .: "logMessage"
        <*> obj .: "databasePath"
        <*> obj .: "markdownPath"
        <*> obj .: "graphPath"
        <*> obj .: "genCssPath"
        <*> obj .: "timetableUrl"
        <*> obj .: "timetableApiUrl"
        <*> obj .: "fasCalendarUrl"
        <*> obj .: "programsUrl"
        <*> obj .: "direction"
        <*> obj .: "divisions"
        <*> obj .: "page"
        <*> obj .: "pageSize"
        <*> obj .: "sessions"
        <*> (parseReqHeaders <$> obj .: "reqHeaders")
        <*> obj .: "fallStartDate"
        <*> obj .: "fallEndDate"
        <*> obj .: "winterStartDate"
        <*> obj .: "winterEndDate"
        <*> obj .: "outDay"
        <*> obj .: "holidaysList"

-- Define a helper structure that matches the JSON format
data HeaderPair = HeaderPair
  { key   :: T.Text
  , value :: T.Text
  }

-- Parse the key-value pair from the JSON
instance FromJSON HeaderPair where
    parseJSON = withObject "HeaderPair" $ \v ->
        HeaderPair <$> v .: "key"
                   <*> v .: "value"

-- Function to convert parsed JSON to RequestHeaders type
parseReqHeaders :: [HeaderPair] -> RequestHeaders
parseReqHeaders = map (\hp -> (packHeaderName (key hp), pack (T.unpack (value hp))))
  where
    packHeaderName :: T.Text -> HeaderName
    packHeaderName = CI.mk . pack . T.unpack

-- Load the configuration
loadConfig :: IO Config
loadConfig = loadYamlSettings ["config.yaml"] [] useEnv

-- SERVER CONFIGURATION

-- | Server configuration settings.
serverConf :: IO Conf
serverConf = do
  config <- loadConfig
  return $ nullConf {
        port      = portValue config,
        logAccess = Just logMAccessShort
    }

-- | Server log configuration. Default is to log access requests using hslogger
-- and a condensed log formatting.
logMAccessShort :: LogAccess t
logMAccessShort host user _ requestLine responseCode _ referer _ = do
    config <- loadConfig
    logM (logMessage config) INFO $ unwords [
        host,
        user,
        requestLine,
        show responseCode,
        referer
        ]


-- DATABASE CONNECTION STRINGS

-- | The path to the database file, relative to the project root.
databasePath :: IO Text
databasePath = databasePathValue <$> loadConfig

-- FILE PATH STRINGS

-- | The relative path to the directory with the markdown files rendered for site content.
markdownPath :: IO String
markdownPath = markdownPathValue <$> loadConfig

-- | The relative path to the directory that contains all of the graph SVG files.
graphPath :: IO String
graphPath = graphPathValue <$> loadConfig

-- | The relative path to the directory containing all of the generated CSS files.
genCssPath :: IO String
genCssPath = do genCssPathValue <$> loadConfig

-- URLs

-- | The URL for U of T's official timetable.
timetableUrl :: IO String
timetableUrl = do timetableUrlValue <$> loadConfig

-- | The Faculty of Arts and Science API for course timetables (by unit).
timetableApiUrl :: IO Text
timetableApiUrl = do timetableApiUrlValue <$> loadConfig

-- | The URLs of the Faculty of Arts & Science calendar.
fasCalendarUrl :: IO String
fasCalendarUrl = do fasCalendarUrlValue <$> loadConfig

programsUrl :: IO String
programsUrl = do programsUrlValue <$> loadConfig

-- HTTP REQUEST STRINGS

-- | Create the body for the HTTP request based on the org
createReqBody :: Text -> IO Value
createReqBody org = do
    config <- loadConfig
    return $ object [ "campuses" .= ([] :: [T.Text]),
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
                        "direction" .= directionValue config,
                        "divisions" .= divisionsValue config,
                        "instructor" .= ("" :: T.Text),
                        "page" .= pageValue config,
                        "pageSize" .= pageSizeValue config,
                        "requirementProps" .= ([] :: [T.Text]),
                        "sessions" .= sessionsValue config,
                        "timePreferences" .= ([] :: [T.Text])
                     ]

-- | The headers for the HTTP request
reqHeaders :: IO RequestHeaders
reqHeaders = do reqHeadersValue <$> loadConfig

-- CALENDAR RESPONSE DATES

-- | First day of classes for the fall term.
fallStartDate :: IO Day
fallStartDate = do fallStartDateValue <$> loadConfig

-- | Last day of classes for the fall term.
fallEndDate :: IO Day
fallEndDate = do fallEndDateValue <$> loadConfig

-- | First day of classes for the winter term.
winterStartDate :: IO Day
winterStartDate = do winterStartDateValue <$> loadConfig

-- | Last day of classes for the winter term.
winterEndDate :: IO Day
winterEndDate = do winterEndDateValue <$> loadConfig

-- | Out of date day. Used to control forbidden inputs for days.
outDay :: IO Day
outDay = do outDayValue <$> loadConfig

-- Holidays for the fall and winter term.
holidays :: IO [String]
holidays = do holidaysList <$> loadConfig
