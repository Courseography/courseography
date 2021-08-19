module Response.Export
    (returnPDF, exportTimetableImageResponse, exportTimetablePDFResponse) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Base64.Lazy as BEnc
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Export.GetImages
import Export.ImageConversion (removeFile)
import Export.LatexGenerator
import Export.PdfGenerator
import Happstack.Server
import Response.Image (returnImageData)

-- | Returns an image of the timetable requested by the user.
exportTimetableImageResponse :: T.Text -> String -> ServerPart Response
exportTimetableImageResponse session selectedCourses = do
    (svgFilename, imageFilename) <- liftIO $ getActiveTimetable (T.pack selectedCourses) session
    liftIO $ returnImageData svgFilename imageFilename

-- | Returns a PDF containing graph and timetable requested by the user.
exportTimetablePDFResponse :: String -> String -> ServerPart Response
exportTimetablePDFResponse selectedCourses graphInfo = do
    (graphSvg, graphImg) <- liftIO $ getActiveGraphImage graphInfo
    (fallsvgFilename, fallimageFilename) <- liftIO $ getActiveTimetable (T.pack selectedCourses) "Fall"
    (springsvgFilename, springimageFilename) <- liftIO $ getActiveTimetable (T.pack selectedCourses) "Spring"
    pdfName <- liftIO $ returnPDF graphSvg graphImg fallsvgFilename fallimageFilename springsvgFilename springimageFilename
    liftIO $ returnPdfBS pdfName

-- | Returns 64base bytestring of PDF for given name, then deletes PDF from local.
returnPdfBS :: String -> IO Response
returnPdfBS pdfFilename = do
    pdfData <- BS.readFile pdfFilename
    _ <- removeFile pdfFilename
    return $ toResponseBS "application/pdf" $ BEnc.encode $ L.fromStrict pdfData

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
