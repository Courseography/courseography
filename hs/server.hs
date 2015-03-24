module Main where

import Control.Monad    (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import GridResponse
import GraphResponse
import DrawResponse
import ImageResponse
import PostResponse
import FourOhFourResponse
import SearchResponse
--import AboutResponse
import Database.CourseQueries (retrieveCourse, allCourses, queryGraphs, courseInfo)
import Css.CssGen
import Filesystem.Path.CurrentOS
import System.Directory
import qualified Data.Text as T
import Diagram

main :: IO ()
main = do
    generateCSS
    cwd <- getCurrentDirectory
    let staticDir = (encodeString $ parent $ decodeString cwd) ++ "public/"
    contents <- readFile "../README.md"
    simpleHTTP nullConf $
        msum [ dir "grid" gridResponse,
               dir "graph" graphResponse,
               dir "draw" drawResponse,
               dir "image" $ graphImageResponse,
               dir "timetable-image" $ look "courses" >>= timetableImageResponse,
               --dir "about" $ aboutResponse contents,
               dir "post" postResponse,
               dir "static" $ serveDirectory EnableBrowsing [] staticDir,
               dir "course" $ look "name" >>= retrieveCourse,
               dir "all-courses" $ liftIO allCourses,
               dir "graphs" $ liftIO queryGraphs,
               dir "course-info" $ liftIO courseInfo,
               dir "timesearch" $ searchResponse,
               fourOhFourResponse
               ]
