{-
Description: Holds constants that could change between the development and production environments.

To make the project compile, this file needs to be named Config.hs to match the module name.
When the project is deployed to the production environment, this file should be swapped with one
containing the production values.
-}

module Config (
    serverConf,
    databasePath,
    runDb,
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
import Network.HTTP.Types.Header (RequestHeaders)
import System.Log.Logger (Priority (INFO), logM)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT, MonadUnliftIO)
import Database.Persist.Sqlite (SqlBackend, runSqlite)
import Control.Monad.IO.Class (liftIO)

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
        <*> obj .: "fallStartDate"
        <*> obj .: "fallEndDate"
        <*> obj .: "winterStartDate"
        <*> obj .: "winterEndDate"
        <*> obj .: "outDay"
        <*> obj .: "holidaysList"

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

-- | Fetch the database path and execute the given action in the context of the database.
runDb :: (MonadUnliftIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDb action = do
  dbPath <- liftIO databasePath
  runSqlite dbPath action


-- FILE PATH STRINGS

-- | The relative path to the directory with the markdown files rendered for site content.
markdownPath :: IO String
markdownPath = markdownPathValue <$> loadConfig

-- | The relative path to the directory that contains all of the graph SVG files.
graphPath :: IO String
graphPath = graphPathValue <$> loadConfig

-- | The relative path to the directory containing all of the generated CSS files.
genCssPath :: IO String
genCssPath = genCssPathValue <$> loadConfig

-- URLs

-- | The URL for U of T's official timetable.
timetableUrl :: IO String
timetableUrl = timetableUrlValue <$> loadConfig

-- | The Faculty of Arts and Science API for course timetables (by unit).
timetableApiUrl :: IO Text
timetableApiUrl = timetableApiUrlValue <$> loadConfig

-- | The URLs of the Faculty of Arts & Science calendar.
fasCalendarUrl :: IO String
fasCalendarUrl = fasCalendarUrlValue <$> loadConfig

programsUrl :: IO String
programsUrl = programsUrlValue <$> loadConfig

-- HTTP REQUEST STRINGS

-- | Create the body for the HTTP request based on the page
createReqBody :: Int -> Value
createReqBody page = object [ "campuses" .= ([] :: [T.Text]),
                       "courseCodeAndTitleProps" .= object
                       [ "courseCode" .= ("" :: T.Text),
                         "courseSectionCode" .= ("" :: T.Text),
                         "courseTitle" .= ("" :: T.Text),
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
                        "page" .= page,
                        "pageSize" .= (300 :: Int),
                        "requirementProps" .= ([] :: [T.Text]),
                        "sessions" .= [T.pack "20249", T.pack "20251", T.pack "20249-20251"],
                        "timePreferences" .= ([] :: [T.Text])
                     ]

-- | The headers for the HTTP request
reqHeaders :: RequestHeaders
reqHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")]

-- CALENDAR RESPONSE DATES

-- | First day of classes for the fall term.
fallStartDate :: IO Day
fallStartDate = fallStartDateValue <$> loadConfig

-- | Last day of classes for the fall term.
fallEndDate :: IO Day
fallEndDate = fallEndDateValue <$> loadConfig

-- | First day of classes for the winter term.
winterStartDate :: IO Day
winterStartDate = winterStartDateValue <$> loadConfig

-- | Last day of classes for the winter term.
winterEndDate :: IO Day
winterEndDate = winterEndDateValue <$> loadConfig

-- | Out of date day. Used to control forbidden inputs for days.
outDay :: IO Day
outDay = outDayValue <$> loadConfig

-- Holidays for the fall and winter term.
holidays :: IO [String]
holidays = holidaysList <$> loadConfig

