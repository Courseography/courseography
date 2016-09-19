 module GetImages
    (getActiveGraphImage, getActiveTimetableImage, getTimetableImage, randomName) where

import TimetableImageCreator (renderTable)
import qualified Data.Map as M
import System.Random
import Svg.Generator
import ImageConversion
import Happstack.Server (Request, rqCookies, cookieValue)
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import Response.Calendar (getInfoCookies, pullDatabase)

-- | If there is an active graph available, an image of that graph is created,
-- otherwise the Computer Science graph is created as a default.
-- Either way, the resulting graph's .svg and .png names are returned.
getActiveGraphImage :: Request -> IO (String, String)
getActiveGraphImage req = do
    let cookies = M.fromList $ rqCookies req
        graphName =
            replace "-" " " $
                maybe "Computer-Science" cookieValue (M.lookup "active-graph" cookies)
    getGraphImage graphName (M.map cookieValue cookies)

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getGraphImage :: String -> M.Map String String -> IO (String, String)
getGraphImage graphName courseMap = do
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    buildSVG graphName courseMap svgFilename True
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Creates an image of a timetable with the currently selected courses and returns the name of the image
getActiveTimetableImage :: Request -> IO (String, String)
getActiveTimetableImage req = do
    let cookies = M.fromList $ rqCookies req
        courses = getInfoCookies (maybe "" cookieValue (M.lookup "selected-lectures" cookies))
        databaseInfo = mapM pullDatabase courses
    -- TODO: Some function that converts course and time data to a timetable image
    return ("", "") -- TODO: replace with svg and image filenames when ready to use this funciton

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getTimetableImage :: String -> String -> IO (String, String)
getTimetableImage courses session = do
    rand <- randomName
    let svgFilename = rand ++ ".svg"
        imageFilename = rand ++ ".png"
    renderTable svgFilename courses session
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Generate a string containing random integers
randomName :: IO String
randomName = do
    gen <- newStdGen
    let (rand, _) = next gen
    return (show rand)