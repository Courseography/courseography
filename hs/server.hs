module Main where

import Control.Monad    (msum)
import Control.Monad.IO.Class  (liftIO)
import Happstack.Server
import Response.GridResponse
import Response.GraphResponse
import Response.DrawResponse
import Response.PostResponse
--import AboutResponse
import Database.CourseQueries
import Css.CssGen
import Filesystem.Path.CurrentOS
import System.Directory
import qualified Data.Text as T


post :: String
post = "post"

main :: IO ()
main = do
    generateCSS
    cwd <- getCurrentDirectory
    let staticDir = encodeString $ parent $ decodeString cwd
    contents <- readFile "../README.md"
    simpleHTTP nullConf $

        msum [ dir "grid" gridResponse,
               dir "graph" graphResponse,
               dir "draw" $ drawResponse,
               --dir "about" $ aboutResponse contents,
               dir "post" $ postResponse,
               dir "static" $ serveDirectory EnableBrowsing [] staticDir,
               dir "course" $ look "name" >>= retrieveCourse, 
               dir "all-courses" $ liftIO allCourses
               ]

retrieveCourse :: String -> ServerPart Response
retrieveCourse course = do

   liftIO $ queryCourse (T.pack course)
