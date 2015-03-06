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
import qualified Data.Map as M

-- | Returns an image of the graph requested by the user.
graphImageResponse :: ServerPart Response
graphImageResponse =
    do req <- askRq
       let cookies = rqCookies req
       liftIO $ getGraphImage $
                M.map cookieValue $ M.fromList cookies

-- | Returns an image of the timetable requested by the user.
timetableImageResponse :: String -> ServerPart Response
timetableImageResponse courses = liftIO $ getTimetableImage courses

-- | Creates an image, and returns the base64 representation of that image.
getGraphImage :: M.Map String String -> IO Response
getGraphImage courseMap = do
	buildSVG courseMap "Testfile2.svg"
	liftIO $ createImageFile "Testfile2.svg" "INSERT_ID-graph.png"
	imageData <- BS.readFile "INSERT_ID-graph.png"
	liftIO $ removeImage "INSERT_ID-graph.png"
	liftIO $ removeImage "Testfile2.svg"
	let encodedData = BEnc.encode imageData
	return $ toResponse encodedData

-- | Creates an image, and returns the base64 representation of that image.
getTimetableImage :: String -> IO Response
getTimetableImage courses =
    do liftIO $ renderTable "circle.svg" courses
       liftIO $ createImageFile "circle.svg" "INSERT_ID-graph.png"
       imageData <- BS.readFile "INSERT_ID-graph.png"
       liftIO $ removeImage "INSERT_ID-graph.png"
       liftIO $ removeImage "Testfile2.svg"
       let encodedData = BEnc.encode imageData
       return $ toResponse encodedData
