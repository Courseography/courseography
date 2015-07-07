module Server
    (runServer) where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Format (FormatTime)
import Happstack.Server hiding (host)
import GridResponse
import GraphResponse
import DrawResponse
import ImageResponse
import PostResponse
import FourOhFourResponse
import SearchResponse
import AboutResponse
import PrivacyResponse
import Database.CourseQueries (retrieveCourse, allCourses, queryGraphs, courseInfo, deptList)
import Filesystem.Path.CurrentOS
import System.Directory
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(LineBuffering))
import System.Log.Logger (logM, updateGlobalLogger, rootLoggerName, setLevel, Priority(INFO))
import FacebookUtilities
import Config (markdownPath)
import qualified Data.Text.Lazy.IO as LazyIO

-- | log access requests using hslogger and a condensed log formatting
logMAccessShort :: FormatTime t => LogAccess t
logMAccessShort host user _ requestLine responseCode _ referer _ =
    logM "Happstack.Server.AccessLog.Combined" INFO $ unwords
        [ host
        , user
        , requestLine
        , show responseCode
        , referer
        ]

runServer :: IO ()
runServer = do
    -- Use line buffering to ensure logging messages are printed correctly
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    -- Set log level to INFO so requests are logged to stdout
    updateGlobalLogger rootLoggerName $ setLevel INFO

    cwd <- getCurrentDirectory
    redirectUrlGraphEmail <- retrieveAuthURL testUrl
    redirectUrlGraphPost <- retrieveAuthURL testPostUrl
    let staticDir = encodeString (parent $ decodeString cwd) ++ "public/"
    aboutContents <- LazyIO.readFile $ markdownPath ++ "README.md"
    privacyContents <- LazyIO.readFile $ markdownPath ++ "PRIVACY.md"

    -- Bind server to PORT environment variable if provided
    portStr <- lookupEnv "PORT"
    let portEnv = read (fromMaybe "8000" portStr) :: Int

    -- Start the HTTP server
    simpleHTTP nullConf {
        port      = portEnv
      , logAccess = Just logMAccessShort
    } $ msum
        [ do
              nullDir
              seeOther "graph" (toResponse "Redirecting to /graph"),
              dir "grid" gridResponse,
              dir "graph" graphResponse,
              dir "image" graphImageResponse,
              dir "timetable-image" $ look "courses" >>= \x -> look "session" >>= timetableImageResponse x,
              dir "graph-fb" $ seeOther redirectUrlGraphEmail $ toResponse "",
              dir "post-fb" $ seeOther redirectUrlGraphPost $ toResponse "",
              dir "test" $ look "code" >>= getEmail,
              dir "test-post" $ look "code" >>= postToFacebook,
              dir "post" postResponse,
              dir "draw" drawResponse,
              dir "about" $ aboutResponse aboutContents,
              dir "privacy" $ privacyResponse privacyContents,
              dir "static" $ serveDirectory EnableBrowsing [] staticDir,
              dir "course" $ look "name" >>= retrieveCourse,
              dir "all-courses" $ liftIO allCourses,
              dir "graphs" $ liftIO queryGraphs,
              dir "course-info" $ look "dept" >>= courseInfo,
              dir "depts" $ liftIO deptList,
              dir "timesearch" searchResponse,
              fourOhFourResponse
        ]
