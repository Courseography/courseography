module Routes
    (routeResponses) where

import Control.Monad (MonadPlus (mplus), msum)
import Controllers.Course as CoursesController (courseInfo, index, retrieveCourse)
import Controllers.Generate as GenerateController (findAndSavePrereqsResponse, generateResponse)
import Controllers.Graph as GraphsController (getGraphJSON, graphImageResponse, graphResponse,
                                              index, saveGraphJSON)
import Controllers.Program as ProgramController (index, retrieveProgram)
import Controllers.Timetable as TimetableController
import Happstack.Server (Browsing (DisableBrowsing), Response, ServerPart, ServerPartT,
                         ToMessage (toResponse), dir, noTrailingSlash, nullDir, seeOther,
                         serveDirectory)
import Response (aboutResponse, drawResponse, loadingResponse, notFoundResponse)

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
    ("post", ProgramController.retrieveProgram),
    ("draw", drawResponse),
    ("about", aboutResponse),
    ("graphs", GraphsController.index),
    ("generate", generateResponse),
    ("get-json-data", getGraphJSON),
    ("course", CoursesController.retrieveCourse),
    ("courses", CoursesController.index),
    ("course-info", CoursesController.courseInfo),
    ("calendar", TimetableController.calendarResponse),
    ("loading", loadingResponse),
    ("save-json", saveGraphJSON),
    ("programs", ProgramController.index)
    ]

strictMatchDir :: (String, ServerPart Response) -> ServerPartT IO Response
strictMatchDir (pathname, response) =
    mplus (do noTrailingSlash       -- enforce no trailing slash in the URI
              dir pathname nullDir  -- enforce that no segments occur after pathname
              response)
          (do dir pathname nullDir  -- if a trailing slash exists, redirect
              seeOther ("/" ++ pathname) (toResponse ("Redirecting to /" ++ pathname)))
