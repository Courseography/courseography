module Routes
    (routeResponses) where

import Control.Monad (MonadPlus (mplus), msum)
import Control.Monad.IO.Class (liftIO)
import Controllers.Course as CourseControllers (retrieveCourse, index, courseInfo, depts)
import Controllers.Graph as GraphControllers
import Data.Text.Lazy (Text)
import Database.CourseInsertion (saveGraphJSON)
import Database.CourseQueries (getGraphJSON, retrievePost)
import Happstack.Server
    ( serveDirectory,
      seeOther,
      dir,
      method,
      noTrailingSlash,
      nullDir,
      look,
      lookBS,
      lookText',
      Browsing(DisableBrowsing),
      ServerPart,
      ServerPartT,
      Method(PUT),
      Response,
      ToMessage(toResponse) )
import Response
    ( drawResponse,
      aboutResponse,
      privacyResponse,
      notFoundResponse,
      searchResponse,
      generateResponse,
      postResponse,
      loadingResponse,
      gridResponse,
      calendarResponse,
      graphImageResponse,
      exportTimetableImageResponse,
      exportTimetablePDFResponse )

routeResponses :: String -> Text -> Text -> ServerPartT IO Response
routeResponses staticDir aboutContents privacyContents =
    msum (map strictMatchDir (strictRoutes aboutContents privacyContents) ++
         [dir "static" $ serveDirectory DisableBrowsing [] staticDir,
          nullDir >> seeOther ("graph" :: String) (toResponse ("Redirecting to /graph" :: String)),
          notFoundResponse])

strictRoutes :: Text -> Text -> [ (String, ServerPart Response)] 
strictRoutes aboutContents privacyContents = [
    ("grid", gridResponse),
    ("graph", GraphControllers.graphResponse),
    ("graph-generate", do method PUT
                          GraphControllers.findAndSavePrereqsResponse),
    ("image", look "JsonLocalStorageObj" >>= graphImageResponse),
    ("timetable-image", lookText' "session" >>= \session -> look "courses" >>= exportTimetableImageResponse session),
    ("timetable-pdf", look "courses" >>= \coursesList -> look "JsonLocalStorageObj" >>= exportTimetablePDFResponse coursesList),
    ("post", retrievePost),
    ("post-progress", postResponse),
    ("draw", drawResponse),
    ("about", aboutResponse aboutContents),
    ("privacy", privacyResponse privacyContents),
    ("graphs", GraphControllers.index),
    ("timesearch", searchResponse),
    ("generate", generateResponse),
    ("get-json-data", lookText' "graphName" >>= \graphName -> liftIO $ getGraphJSON graphName),
    
    ("course", lookText' "name" >>= CourseControllers.retrieveCourse),
    ("courses", CourseControllers.index),
    ("course-info", lookText' "dept" >>= CourseControllers.courseInfo),
    ("depts", CourseControllers.depts),
    ("calendar", look "courses" >>= calendarResponse),
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
