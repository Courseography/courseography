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
import SvgParsing.SVGGenerator
import SvgParsing.ParserUtil
import Diagram (renderTable)
import Control.Concurrent
import qualified Data.Map as M

-- | Returns an image of the graph requested by the user.
graphImageResponse :: MVar Integer -> ServerPart Response
graphImageResponse counter =
    do req <- askRq
       let cookies = rqCookies req
       liftIO $ getGraphImage counter (M.map cookieValue $ M.fromList cookies)

-- | Returns an image of the timetable requested by the user.
timetableImageResponse :: MVar Integer -> String -> ServerPart Response
timetableImageResponse counter courses =
   liftIO $ getTimetableImage counter courses

-- | Creates an image, and returns the base64 representation of that image.
getGraphImage :: MVar Integer -> M.Map String String -> IO Response
getGraphImage counter courseMap =
    do c <- addCounter counter
       let svgFilename = (show c ++ "-graph-svg-file.svg")
           imageFilename = (show c ++ "-graph.png")
       buildSVG courseMap svgFilename
       liftIO $ createImageFile svgFilename imageFilename
       imageData <- BS.readFile imageFilename
       liftIO $ removeImage imageFilename
       liftIO $ removeImage svgFilename
       let encodedData = BEnc.encode imageData
       return $ toResponse encodedData

-- | Creates an image, and returns the base64 representation of that image.
getTimetableImage :: MVar Integer -> String -> IO Response
getTimetableImage counter courses =
    do c <- addCounter counter
       let svgFilename = (show c ++ "--timetable-svg-file.svg")
           imageFilename = (show c ++ "-timetable.png")
       liftIO $ renderTable svgFilename courses
       liftIO $ createImageFile svgFilename imageFilename
       imageData <- BS.readFile imageFilename
       liftIO $ removeImage imageFilename
       liftIO $ removeImage svgFilename
       let encodedData = BEnc.encode imageData
       return $ toResponse encodedData

addCounter :: MVar Integer -> IO Integer
addCounter counter = do
    c <- takeMVar counter
    liftIO $ (putMVar counter . (+) 1) c
    return c
