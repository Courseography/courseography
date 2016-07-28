{-# LANGUAGE OverloadedStrings #-}

module Response.Image
    (graphImageResponse, timetableImageResponse) where

import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Base64.Lazy as BEnc
import ImageConversion
import Svg.Generator
import TimetableImageCreator (renderTable)
import qualified Data.Map as M
import System.Random
import Database.Tables (GraphId)
import Database.Persist.Sql (toSqlKey)
import Data.Int(Int64)
import Data.List.Utils (replace)

-- | Returns an image of the graph requested by the user.
graphImageResponse :: ServerPart Response
graphImageResponse = do
    req <- askRq
    let cookies = M.fromList $ rqCookies req
        graphName =
            replace "-" " " $
                maybe "Computer-Science" cookieValue (M.lookup "active-graph" cookies)
    liftIO $ print $ "Generating image for " ++ graphName
    liftIO $ getGraphImage graphName (M.map cookieValue cookies)

-- | Returns an image of the timetable requested by the user.
timetableImageResponse :: String -> String -> ServerPart Response
timetableImageResponse courses session =
    liftIO $ getTimetableImage courses session

-- | Creates an image, and returns the base64 representation of that image.
getGraphImage :: String -> M.Map String String -> IO Response
getGraphImage graphName courseMap = do
    gen <- newStdGen
    let (rand, _) = next gen
        svgFilename = show rand ++ ".svg"
        imageFilename = show rand ++ ".png"
    buildSVG graphName courseMap svgFilename True
    returnImageData svgFilename imageFilename

-- | Creates an image, and returns the base64 representation of that image.
getTimetableImage :: String -> String -> IO Response
getTimetableImage courses session = do
    gen <- newStdGen
    let (rand, _) = next gen
        svgFilename = show rand ++ ".svg"
        imageFilename = show rand ++ ".png"
    renderTable svgFilename courses session
    returnImageData svgFilename imageFilename

-- | Creates and converts an SVG file to an image file, deletes them both and
-- returns the image data as a response.
returnImageData :: String -> String -> IO Response
returnImageData svgFilename imageFilename = do
    createImageFile svgFilename imageFilename
    imageData <- BS.readFile imageFilename
    removeImage imageFilename
    removeImage svgFilename
    let encodedData = BEnc.encode imageData
    return $ toResponse encodedData
