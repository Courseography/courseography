module Routes
    (routeResponses) where

import Control.Monad (MonadPlus (mplus), msum)
import Controllers.Course as CoursesController (retrieveCourse, index, courseInfo, depts)
import Controllers.Graph as GraphsController
    ( graphResponse, index, getGraphJSON, graphImageResponse )
import Controllers.Generate as GenerateController (generateResponse, findAndSavePrereqsResponse)
import Controllers.Timetable as TimetableController
import Database.CourseInsertion (saveGraphJSON)
import Database.CourseQueries (retrievePost)
import Happstack.Server
    ( serveDirectory,
      seeOther,
      dir,
      noTrailingSlash,
      nullDir,
      Browsing(DisableBrowsing),
      ServerPart,
      ServerPartT,
      Response,
      ToMessage(toResponse) )
import Response
    ( drawResponse,
      aboutResponse,
      notFoundResponse,
      searchResponse,
      postResponse,
      loadingResponse)

routeResponses :: String -> ServerPartT IO Response
routeResponses staticDir =
    msum (map strictMatchDir strictRoutes ++
         [dir "static" $ serveDirectory DisableBrowsing [] staticDir,
          nullDir >> seeOther ("graph" :: String) (toResponse ("Redirecting to /graph" :: String)),
          notFoundResponse])

strictRoutes :: [(String, ServerPart Response)] 
strictRoutes = [
    ("grid", TimetableController.gridResponse),
    ("graph", GraphsController.graphResponse),
    ("graph-generate", GenerateController.findAndSavePrereqsResponse),
    ("image", graphImageResponse),
    ("timetable-image", TimetableController.exportTimetableImageResponse),
    ("timetable-pdf", TimetableController.exportTimetablePDFResponse),
    ("post", retrievePost),
    ("post-progress", postResponse),
    ("draw", drawResponse),
    ("about", aboutResponse),
    ("graphs", GraphsController.index),
    ("timesearch", searchResponse),
    ("generate", generateResponse),
    ("get-json-data", getGraphJSON),
    ("course", CoursesController.retrieveCourse),
    ("courses", CoursesController.index),
    ("course-info", CoursesController.courseInfo),
    ("depts", CoursesController.depts),
    ("calendar", TimetableController.calendarResponse),
    ("loading", loadingResponse),
    ("save-json", saveGraphJSON)
    ]

strictMatchDir :: (String, ServerPart Response) -> ServerPartT IO Response
strictMatchDir (pathname, response) =
    mplus (do noTrailingSlash       -- enforce no trailing slash in the URI
              dir pathname nullDir  -- enforce that no segments occur after pathname
              response)
          (do dir pathname nullDir  -- if a trailing slash exists, redirect
              seeOther ("/" ++ pathname) (toResponse ("Redirecting to /" ++ pathname)))
