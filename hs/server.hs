module Main where

import Control.Monad    (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import GridResponse
import GraphResponse
import DrawResponse
import ImageResponse
import PostResponse
import ImageResponse
--import AboutResponse
import Database.CourseQueries
import Css.CssGen
import Filesystem.Path.CurrentOS
import System.Directory
import CourseographyFacebook
import qualified Data.Text as T
import Diagram

main :: IO ()
main = do
    generateCSS
    cwd <- getCurrentDirectory
    let staticDir = encodeString $ parent $ decodeString cwd
    redirectUrlGraphEmail <- retrieveAuthURL testUrl
    redirectUrlGraphPost <- retrieveAuthURL testPostUrl
    contents <- readFile "../README.md"
    print "Server is running..."
    simpleHTTP nullConf $
        msum [ dir "grid" gridResponse,
               dir "graph" graphResponse,
               dir "image" $ imageResponse,
               dir "graph-fb" $ seeOther redirectUrlGraphEmail $ toResponse "",
               dir "post-fb" $ seeOther redirectUrlGraphPost $ toResponse "",
               dir "test" $ look "code" >>= getEmail,
               dir "test-post" $ look "code" >>= postToFacebook,
               dir "draw" $ drawResponse,
               --dir "about" $ aboutResponse contents,
               dir "static" $ serveDirectory EnableBrowsing [] staticDir,
               dir "course" $ look "name" >>= retrieveCourse,
               dir "all-courses" $ liftIO allCourses,
               dir "svg" $ look "courses" >>= svgResponse
             ]

retrieveCourse :: String -> ServerPart Response
retrieveCourse course =
   liftIO $ queryCourse (T.pack course)

svgResponse :: String -> ServerPart Response
svgResponse courses = do
  liftIO $ renderTable courses
  -- Right now serving the file, but the client isn't doing anthing with it
  serveFile (asContentType "image/svg+xml") "circle.svg"
