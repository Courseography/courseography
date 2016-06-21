module Route
    (route) where

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
    import Data.Int (Int64)


route = 
[ ("grid", gridResponse),
  ("graph", graphResponse),
  ("image", graphImageResponse),
  ("timetable-image", timetableImageResponse x),
  ("graph-fb",toResponse ""),
  ("post-fb",toResponse ""),
  ("test", getEmail),
  ("test-post", postToFacebook),
  ("post", postResponse),
  ("draw", drawResponse),                  
  ("about", aboutResponse),
  ("privacy" ,privacyResponse),
  ("static", serveDirectory),
  ("course", retrieveCourse),
  ("all-courses", allCourses),
  ("graphs", queryGraphs),
  ("course-info", courseInfo),
  ( "depts", deptList),
  ("timesearch",searchResponse),
  ("calendar",calendarResponse),
  ("get-json-data",getGraphJSON),
  ("loading",loadingResponse),
  ("save-json", saveGraphJSON),
  (x, notFoundResponse)]
