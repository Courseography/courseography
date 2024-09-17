module Response.Image
    (timetableImageResponse, returnImageData) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BEnc
import qualified Data.Text as T
import Export.GetImages (getTimetableImage)
import Happstack.Server
import System.Directory (removeFile)

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
