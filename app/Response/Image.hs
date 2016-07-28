{-# LANGUAGE OverloadedStrings #-}

module Response.Image
    (graphImageResponse, timetableImageResponse) where

import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Base64.Lazy as BEnc
import qualified Data.Map as M
import GetImages
import ImageConversion
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
    (svgFilename, imageFilename) <- liftIO $ getGraphImage graphName (M.map cookieValue cookies)
    liftIO $ returnImageData svgFilename imageFilename

-- | Returns an image of the timetable requested by the user.
timetableImageResponse :: String -> String -> ServerPart Response
timetableImageResponse courses session = do
    (svgFilename, imageFilename) <- liftIO $ getTimetableImage courses session
    liftIO $ returnImageData svgFilename imageFilename

-- | Creates and converts an SVG file to an image file, deletes them both and
-- returns the image data as a response.
returnImageData :: String -> String -> IO Response
returnImageData svgFilename imageFilename = do
    imageData <- BS.readFile imageFilename
    removeImage imageFilename
    removeImage svgFilename
    let encodedData = BEnc.encode imageData
    return $ toResponse encodedData
