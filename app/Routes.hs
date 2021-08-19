module Routes
    (routeResponses) where

import Control.Monad (MonadPlus (mplus), msum)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Database.CourseInsertion (saveGraphJSON)
import Database.CourseQueries (allCourses, courseInfo, deptList, getGraphJSON, queryGraphs,
                               retrieveCourse)
import DynamicGraphs.WriteRunDot (findAndSavePrereqsResponse)
import Happstack.Server hiding (host)
import Response

routeResponses :: String -> Text -> Text -> ServerPartT IO Response
routeResponses staticDir aboutContents privacyContents =
    msum (map strictMatchDir (strictRoutes aboutContents privacyContents) ++
         [dir "static" $ serveDirectory DisableBrowsing [] staticDir,
          nullDir >> seeOther ("graph" :: String) (toResponse ("Redirecting to /graph" :: String)),
          notFoundResponse])

strictRoutes :: Text -> Text -> [ (String, ServerPart Response)]
strictRoutes aboutContents privacyContents = [
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

strictMatchDir :: (String, ServerPart Response) -> ServerPartT IO Response
strictMatchDir (pathname, response) =
    mplus (do noTrailingSlash       -- enforce no trailing slash in the URI
              dir pathname nullDir  -- enforce that no segments occur after pathname
              response)
          (do dir pathname nullDir  -- if a trailing slash exists, redirect
              seeOther ("/" ++ pathname) (toResponse ("Redirecting to /" ++ pathname)))
