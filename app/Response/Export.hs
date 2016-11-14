{-# LANGUAGE OverloadedStrings #-}
module Response.Export
    (returnPDF, exportTimetableResponse, exportTimetablePDFResponse, returnPdfData) where

import Control.Monad.IO.Class  (liftIO)
import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import Export.GetImages
import Export.ImageConversion (removeFile)
import Export.PdfGenerator
import Export.LatexGenerator
import Response.Image (returnImageData)

-- get timetable from parsing selected-lectures cookie
-- If using returnImageData, can download timetable images to local.
-- Elseif using return pdf, suppose to get pdf of timetable
exportTimetableResponse :: String -> ServerPart Response
exportTimetableResponse session = do
    req <- askRq
    (svgFilename, imageFilename) <- liftIO $ getActiveTimetable req session
    liftIO $ returnImageData svgFilename imageFilename

exportTimetablePDFResponse :: ServerPart Response
exportTimetablePDFResponse = do
    req <- askRq
    (graphSvg, graphImg) <- liftIO $ getActiveGraphImage req
    (fallsvgFilename, fallimageFilename) <- liftIO $ getActiveTimetable req "Fall"
    (springsvgFilename, springimageFilename) <- liftIO $ getActiveTimetable req "Spring"
    pdfName <- liftIO $ returnPDF graphSvg graphImg fallsvgFilename fallimageFilename springsvgFilename springimageFilename
    liftIO $ returnPdfData pdfName

-- | Read PDF and convert into bytestring formate, then delete from local
returnPdfData :: String -> IO Response
returnPdfData pdfFilename = do
    pdfData <- BS.readFile pdfFilename
    _ <- removeFile pdfFilename
    return $ toResponseBS "application/pdf" pdfData

-- | Returns the name of a generated pdf that contains graphImg and timetableImg
-- and deletes all of the img and svg files passed as arguments
returnPDF :: String -> String -> String -> String -> String -> String -> IO String
returnPDF graphSvg graphImg fallTimetableSvg fallTimetableImg springTimetableSvg springTimetableImg = do
    rand <- randomName
    let texName = rand ++ ".tex"
        pdfName = rand ++ ".pdf"
    generateTex [graphImg, fallTimetableImg, springTimetableImg] texName -- generate a temporary TEX file
    createPDF texName                            -- create PDF using TEX and delete the TEX file afterwards
    _ <- removeFile (graphSvg ++ " " ++ graphImg ++ " " ++ fallTimetableSvg ++ " " ++ fallTimetableImg ++ " " ++ springTimetableSvg ++ " " ++ springTimetableImg)
    return pdfName
