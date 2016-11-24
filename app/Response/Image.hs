{-# LANGUAGE OverloadedStrings #-}

module Response.Image
    (graphImageResponse, timetableImageResponse, timetableImageCookieResponse, timetablePDFResponse) where

import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Base64.Lazy as BEnc
import Export.GetImages
import Export.ImageConversion

import Response.Export (returnPDF)


-- | Returns an image of the graph requested by the user.
graphImageResponse :: ServerPart Response
graphImageResponse = do
    req <- askRq
    (svgFilename, imageFilename) <- liftIO $ getActiveGraphImage req
    liftIO $ returnImageData svgFilename imageFilename

-- | Returns an image of the timetable requested by the user.
timetableImageResponse :: String -> String -> ServerPart Response
timetableImageResponse courses session = do
    (svgFilename, imageFilename) <- liftIO $ getTimetableImage courses session
    liftIO $ returnImageData svgFilename imageFilename

-- get timetable from parsing selected-lectures cookie
-- If using returnImageData, can download timetable images to local.
-- Elseif using return pdf, suppose to get pdf of timetable
timetableImageCookieResponse :: String -> ServerPart Response
timetableImageCookieResponse session = do
    req <- askRq
    (svgFilename, imageFilename) <- liftIO $ getActiveTimetable req session
    liftIO $ returnImageData svgFilename imageFilename

timetablePDFResponse :: ServerPart Response
timetablePDFResponse = do
    req <- askRq
    (graphSvg, graphImg) <- liftIO $ getActiveGraphImage req
    (fallsvgFilename, fallimageFilename) <- liftIO $ getActiveTimetable req "Fall"
    (springsvgFilename, springimageFilename) <- liftIO $ getActiveTimetable req "Spring"
    pdfName <- liftIO $ returnPDF graphSvg graphImg fallsvgFilename fallimageFilename springsvgFilename springimageFilename
    liftIO $ returnPdfData pdfName

-- | Creates and converts an SVG file to an image file, deletes them both and
-- returns the image data as a response.
returnImageData :: String -> String -> IO Response
returnImageData svgFilename imageFilename = do
    imageData <- BS.readFile imageFilename
    _ <- removeImage imageFilename
    _ <- removeImage svgFilename
    let encodedData = BEnc.encode imageData
    return $ toResponse encodedData

-- | Read PDF and convert into bytestring formate, then delete from local
returnPdfData :: String -> IO Response
returnPdfData pdfFilename = do
    pdfData <- BS.readFile pdfFilename
    _ <- removeImage pdfFilename
    return $ toResponseBS "application/pdf" pdfData
