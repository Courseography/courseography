{-# LANGUAGE OverloadedStrings #-}

module ImageResponse where

import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Base64.Lazy as BEnc
import ImageConversion
import Svg.Generator
import Diagram (renderTable)
import qualified Data.Map as M
import System.Random
import Database.Tables (GraphId)
import Database.Persist.Sql (toSqlKey)
import Data.Int(Int64)

-- | Returns an image of the graph requested by the user.
graphImageResponse :: ServerPart Response
graphImageResponse =
    do req <- askRq
       let cookies = M.fromList $ rqCookies req
           gId = maybe "1" cookieValue
                         (M.lookup "active-graph" cookies)
           graphKey = read gId :: Int64
       liftIO $ print graphKey
       liftIO $ getGraphImage (toSqlKey graphKey) (M.map cookieValue cookies)

-- | Returns an image of the timetable requested by the user.
timetableImageResponse :: String -> String -> ServerPart Response
timetableImageResponse courses session =
    liftIO $ getTimetableImage courses session

-- | Creates an image, and returns the base64 representation of that image.
getGraphImage :: GraphId -> M.Map String String -> IO Response
getGraphImage gId courseMap = do
    gen <- newStdGen
    let (rand, _) = next gen
        svgFilename = (show rand ++ ".svg")
        imageFilename = (show rand ++ ".png")
    buildSVG gId courseMap svgFilename True
    returnImageData svgFilename imageFilename

-- | Creates an image, and returns the base64 representation of that image.
getTimetableImage :: String -> String -> IO Response
getTimetableImage courses session =
    do gen <- newStdGen
       let (rand, _) = next gen
           svgFilename = (show rand ++ ".svg")
           imageFilename = (show rand ++ ".png")
       renderTable svgFilename courses session
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
