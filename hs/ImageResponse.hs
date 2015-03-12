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
           svgFilename = (show rand ++ "-graph-svg-file.svg")
           imageFilename = (show rand ++ "-graph.png")
       buildSVG courseMap svgFilename
       createImageFile svgFilename imageFilename
       imageData <- BS.readFile imageFilename
       removeImage imageFilename
       removeImage svgFilename
       let encodedData = BEnc.encode imageData
       return $ toResponse encodedData

-- | Creates an image, and returns the base64 representation of that image.
getTimetableImage :: String -> IO Response
getTimetableImage courses =
    do gen <- newStdGen
       let (rand, _) = next gen
           svgFilename = (show rand ++ "--timetable-svg-file.svg")
           imageFilename = (show rand ++ "-timetable.png")
       renderTable svgFilename courses
       createImageFile svgFilename imageFilename
       imageData <- BS.readFile imageFilename
       removeImage imageFilename
       removeImage svgFilename
       let encodedData = BEnc.encode imageData
       return $ toResponse encodedData
