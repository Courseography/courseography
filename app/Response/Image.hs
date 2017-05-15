module Response.Image
    (graphImageResponse, timetableImageResponse, returnImageData) where

import Happstack.Server
import qualified Data.ByteString as BS
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Base64 as BEnc
import Export.GetImages
import Export.ImageConversion
import qualified Data.Text as T

-- | Returns an image of the graph requested by the user.
graphImageResponse :: ServerPart Response
graphImageResponse = do
    req <- askRq
    (svgFilename, imageFilename) <- liftIO $ getActiveGraphImage req
    liftIO $ returnImageData svgFilename imageFilename

-- | Returns an image of the timetable requested by the user.
timetableImageResponse :: T.Text -> T.Text -> ServerPart Response
timetableImageResponse courses session = do
    (svgFilename, imageFilename) <- liftIO $ getTimetableImage courses session
    liftIO $ returnImageData svgFilename imageFilename

-- | Creates and converts an SVG file to an image file, deletes them both and
-- returns the image data as a response.
returnImageData :: String -> String -> IO Response
returnImageData svgFilename imageFilename = do
    imageData <- BS.readFile imageFilename
    _ <- removeFile imageFilename
    _ <- removeFile svgFilename
    let encodedData = BEnc.encode imageData
    return $ toResponse encodedData
