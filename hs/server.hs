module Main where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
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
import Css.CssGen
import Filesystem.Path.CurrentOS
import System.Directory
import CourseographyFacebook
import qualified Data.Text as T

main :: IO ()
main = do
    generateCSS
    cwd <- getCurrentDirectory
    redirectUrlGraphEmail <- retrieveAuthURL testUrl
    redirectUrlGraphPost <- retrieveAuthURL testPostUrl
    let staticDir = encodeString (parent $ decodeString cwd) ++ "public/"
    aboutContents <- readFile "../README.md"
    privacyContents <- readFile "../PRIVACY.md"
    print "Server is running..."
    simpleHTTP nullConf $ msum
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
