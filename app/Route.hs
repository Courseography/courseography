module Route
    (routes) where

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server hiding (host)
import Response
import Database.CourseQueries (retrieveCourse, allCourses, queryGraphs, courseInfo, deptList, getGraphJSON)
import Database.CourseInsertion (saveGraphJSON)
import Filesystem.Path.CurrentOS as Path
import System.Directory (getCurrentDirectory)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(LineBuffering))
import System.Log.Logger (updateGlobalLogger, rootLoggerName, setLevel, Priority(INFO))
import Data.String (fromString)
import FacebookUtilities
import Config (markdownPath, serverConf)
import qualified Data.Text.Lazy.IO as LazyIO
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Data.Int (Int64)

routes :: [Char] -> T.Text -> T.Text -> Text -> Text -> [ (String, ServerPart Response)]
routes staticDir redirectUrlGraphEmail redirectUrlGraphPost aboutContents privacyContents = [
    ("grid", gridResponse),
    ("graph", graphResponse),
    ("image", graphImageResponse),
    ("timetable-image", look "courses" >>= \x -> look "session" >>= timetableImageResponse x),
    ("graph-fb", seeOther redirectUrlGraphEmail $ toResponse ""),
    ("post-fb", seeOther redirectUrlGraphPost $ toResponse ""),
    ("test", look "code" >>= getEmail),
    ("test-post", look "code" >>= postToFacebook),
    ("post", postResponse),
    ("draw", drawResponse),                  
    ("about", aboutResponse aboutContents),
    ("privacy", privacyResponse privacyContents),
    ("static", serveDirectory DisableBrowsing [] staticDir),
    ("course", look "name" >>= retrieveCourse),
    ("all-courses", liftIO allCourses),
    ("graphs", liftIO queryGraphs),
    ("course-info", look "dept" >>= courseInfo),
    ("depts", liftIO deptList),
    ("timesearch", searchResponse),
    ("calendar", lookCookieValue "selected-lectures" >>= calendarResponse),
    ("get-json-data", look "graphName" >>= \graphName -> liftIO $ getGraphJSON graphName),
    ("loading", look "size" >>= loadingResponse),
    ("save-json", look "jsonData" >>= \jsonStr -> look "nameData" >>= \nameStr -> liftIO $ saveGraphJSON jsonStr nameStr)]

