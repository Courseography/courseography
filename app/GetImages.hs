 module GetImages 
 	(getGraphImage, getTimetableImage) where

import TimetableImageCreator (renderTable)
import qualified Data.Map as M
import System.Random
import Svg.Generator
import ImageConversion

 -- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getGraphImage :: String -> M.Map String String -> IO (String, String)
getGraphImage graphName courseMap = do
    gen <- newStdGen
    let (rand, _) = next gen
        svgFilename = show rand ++ ".svg"
        imageFilename = show rand ++ ".png"
    buildSVG graphName courseMap svgFilename True
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)

-- | Creates an image, and returns the name of the svg used to create the
-- image and the name of the image
getTimetableImage :: String -> String -> IO (String, String)
getTimetableImage courses session = do
    gen <- newStdGen
    let (rand, _) = next gen
        svgFilename = show rand ++ ".svg"
        imageFilename = show rand ++ ".png"
    renderTable svgFilename courses session
    createImageFile svgFilename imageFilename
    return (svgFilename, imageFilename)