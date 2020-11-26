module Routes
    (routes) where

import Control.Monad.IO.Class (liftIO)
import Happstack.Server hiding (host)
import Response
import Database.CourseQueries (retrieveCourse, allCourses, queryGraphs, courseInfo, deptList, getGraphJSON)
import Database.CourseInsertion (saveGraphJSON)
import Data.Text.Lazy (Text)
import DynamicGraphs.WriteRunDot (findAndSavePrereqsResponse)

routes :: String -> Text -> Text -> [ (String, ServerPart Response)]
routes staticDir aboutContents privacyContents = [
    ("grid", gridResponse),
    ("graph", graphResponse),
    ("graph-generate", do method PUT
                          findAndSavePrereqsResponse),
    ("image", look "JsonLocalStorageObj" >>= graphImageResponse),
    ("timetable-image", lookText' "session" >>= \session -> look "courses" >>= exportTimetableImageResponse session),
    ("timetable-pdf", look "courses" >>= \courses -> look "JsonLocalStorageObj" >>= exportTimetablePDFResponse courses),
    ("post", postResponse),
    ("draw", drawResponse),
    ("about", aboutResponse aboutContents),
    ("privacy", privacyResponse privacyContents),
    ("static", serveDirectory DisableBrowsing [] staticDir),
    ("course", lookText' "name" >>= retrieveCourse),
    ("all-courses", liftIO allCourses),
    ("graphs", liftIO queryGraphs),
    ("course-info", lookText' "dept" >>= courseInfo),
    ("depts", liftIO deptList),
    ("timesearch", searchResponse),
    ("generate", generateResponse),
    ("calendar", look "courses" >>= calendarResponse),
    ("get-json-data", lookText' "graphName" >>= \graphName -> liftIO $ getGraphJSON graphName),
    ("loading", lookText' "size" >>= loadingResponse),
    ("save-json", lookBS "jsonData" >>= \jsonStr -> lookText' "nameData" >>= \nameStr -> liftIO $ saveGraphJSON jsonStr nameStr)
    ]
