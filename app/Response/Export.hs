{-# LANGUAGE OverloadedStrings #-}
module Response.Export
    (returnPDF, exportTimetableImageResponse, exportTimetablePDFResponse) where

import Control.Monad.IO.Class (liftIO)
import Happstack.Server
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Export.GetImages
import Export.ImageConversion (removeFile)
import Export.PdfGenerator
import Export.LatexGenerator
import Response.Image (returnImageData)
import qualified Data.Text as T

-- | Returns an image of the timetable requested by the user.
exportTimetableImageResponse :: T.Text -> String -> ServerPart Response
exportTimetableImageResponse session coursecookie = do
    (svgFilename, imageFilename) <- liftIO $ getActiveTimetable (T.pack coursecookie) session
    liftIO $ returnImageData svgFilename imageFilename

-- | Returns a PDF containing graph and timetable requested by the user.
exportTimetablePDFResponse :: String -> ServerPart Response
exportTimetablePDFResponse coursecookie = do
    req <- askRq
    (graphSvg, graphImg) <- liftIO $ getActiveGraphImage req
    (fallsvgFilename, fallimageFilename) <- liftIO $ getActiveTimetable (T.pack coursecookie) "Fall"
    (springsvgFilename, springimageFilename) <- liftIO $ getActiveTimetable (T.pack coursecookie) "Spring"
    pdfName <- liftIO $ returnPDF graphSvg graphImg fallsvgFilename fallimageFilename springsvgFilename springimageFilename
    liftIO $ returnPdfBS pdfName

-- | Returns bytestring of PDF for given name, then deletes PDF from local.
returnPdfBS :: String -> IO Response
returnPdfBS pdfFilename = do
    pdfData <- BS.readFile pdfFilename
    _ <- removeFile pdfFilename
    return $ toResponseBS "application/pdf" $ L.fromStrict pdfData

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
