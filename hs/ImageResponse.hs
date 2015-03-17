{-# LANGUAGE OverloadedStrings #-}

module ImageResponse where

import Data.List
import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as B
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Base64.Lazy as BEnc
import ImageConversion
import Data.List.Split
import SvgParsing.Generator
import SvgParsing.ParserUtil
import Diagram (renderTable)
import qualified Data.Map as M
import System.Random

-- | Returns an image of the graph requested by the user.
graphImageResponse :: ServerPart Response
graphImageResponse =
    do req <- askRq
       let cookies = rqCookies req
       liftIO $ getGraphImage (M.map cookieValue $ M.fromList cookies)

-- | Returns an image of the timetable requested by the user.
timetableImageResponse :: String -> ServerPart Response
timetableImageResponse courses =
   liftIO $ getTimetableImage courses

-- | Creates an image, and returns the base64 representation of that image.
getGraphImage :: M.Map String String -> IO Response
getGraphImage courseMap =
    do gen <- newStdGen
       let (rand, _) = next gen
           svgFilename = (show rand ++ ".svg")
           imageFilename = (show rand ++ ".png")
       buildSVG 1 courseMap svgFilename
       returnImageData svgFilename imageFilename

-- | Creates an image, and returns the base64 representation of that image.
getTimetableImage :: String -> IO Response
getTimetableImage courses =
    do gen <- newStdGen
       let (rand, _) = next gen
           svgFilename = (show rand ++ ".svg")
           imageFilename = (show rand ++ ".png")
       renderTable svgFilename courses
       returnImageData svgFilename imageFilename

-- | Creates and converts an SVG file to an image file, deletes them both and
-- returns the image data as a response.
returnImageData :: String -> String -> IO Response
returnImageData svgFilename imageFilename =
    do createImageFile svgFilename imageFilename
       imageData <- BS.readFile imageFilename
       removeImage imageFilename
       removeImage svgFilename
       let encodedData = BEnc.encode imageData
       return $ toResponse encodedData
