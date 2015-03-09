module Main where

import Control.Monad    (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import GridResponse
import GraphResponse
import DrawResponse
import ImageResponse
import PostResponse
--import AboutResponse
import Database.CourseQueries
import Css.CssGen
import Filesystem.Path.CurrentOS
import System.Directory
import qualified Data.Text as T
import Diagram
import Control.Concurrent

main :: IO ()
main = do
    generateCSS
    cwd <- getCurrentDirectory
    let staticDir = encodeString $ parent $ decodeString cwd
    contents <- readFile "../README.md"
    counter <- (newMVar 0) :: IO (MVar Integer)
    simpleHTTP nullConf $
        msum [ dir "grid" gridResponse,
               dir "graph" graphResponse,
               dir "draw" drawResponse,
               dir "image" $ graphImageResponse counter,
               dir "timetable-image" $ look "courses" >>= (timetableImageResponse counter),
               --dir "about" $ aboutResponse contents,
               dir "post" postResponse,
               dir "static" $ serveDirectory EnableBrowsing [] staticDir,
               dir "course" $ look "name" >>= retrieveCourse,
               dir "all-courses" $ liftIO allCourses
               ]

retrieveCourse :: String -> ServerPart Response
retrieveCourse course =
    liftIO $ queryCourse (T.pack course)
