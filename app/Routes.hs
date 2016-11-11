module Routes
    (routes) where

import Control.Monad.IO.Class (liftIO)
import Happstack.Server hiding (host)
import Response
import Database.CourseQueries (retrieveCourse, allCourses, queryGraphs, courseInfo, deptList, getGraphJSON)
import Database.CourseInsertion (saveGraphJSON)
import Data.Text.Lazy (Text)

routes :: [Char] -> Text -> Text -> [ (String, ServerPart Response)]
routes staticDir aboutContents privacyContents = [
    ("grid", gridResponse),
    ("graph", graphResponse),
    ("image", graphImageResponse),
    ("timetable-image", look "session" >>= timetableImageCookieResponse),
    ("timetable-pdf", timetablePDFResponse),
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
    ("save-json", look "jsonData" >>= \jsonStr -> look "nameData" >>= \nameStr -> liftIO $ saveGraphJSON jsonStr nameStr)
    ]
