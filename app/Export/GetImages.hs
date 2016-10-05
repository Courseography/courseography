 module Export.GetImages
    (getActiveGraphImage, getTimetableImage, randomName, getActiveTimetable) where

import Export.TimetableImageCreator (renderTable)
import qualified Data.Map as M
import System.Random
import Svg.Generator
import Export.ImageConversion
import Happstack.Server (Request, rqCookies, cookieValue)
import Data.List.Utils (replace)

-- | If there is an active graph available, an image of that graph is created,
-- otherwise the Computer Science graph is created as a default.
-- Either way, the resulting graph's .svg and .png names are returned.
getActiveGraphImage :: Request -> IO (String, String)
getActiveGraphImage req = do
    let cookies = M.fromList $ rqCookies req  --  Map String Cookie
        graphName =
            replace "-" " " $
                maybe "Computer-Science" cookieValue (M.lookup "active-graph" cookies)  -- if M.lookup "active-graph" cookies is Nothing, then "Computer Science", else get cookieValue of coookie
    getGraphImage graphName (M.map cookieValue cookies)


-- =================================
-- Get lectures selected by user from selected-lectures cookie. If DNE, assume no lecture seleted
getActiveTimetable :: Request -> IO (String)
getActiveTimetable req = do
    let cookies = M.fromList $ rqCookies req  --  Map String Cookie
        coursecookie = maybe "" cookieValue $ M.lookup "selected-lectures" cookies
    return coursecookie




-- =================================

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
