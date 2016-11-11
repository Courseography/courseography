module Response.Export
    (exportGraphResponse, returnPDF) where

import Control.Monad.IO.Class  (liftIO)
import Happstack.Server
import Export.GetImages
import Export.ImageConversion (removeImage)
import Export.PdfGenerator
import Export.LatexGenerator

-- | Serves a pdf file that includes a graph and timetable information from
-- selected course sessions
exportGraphResponse :: String -> String -> ServerPart Response
exportGraphResponse courses session = do
    req <- askRq
    (graphSvg, graphImg) <- liftIO $ getActiveGraphImage req
    (fallsvgFilename, fallimageFilename) <- liftIO $ getActiveTimetable req "Fall"
    (springsvgFilename, springimageFilename) <- liftIO $ getActiveTimetable req "Spring"
    pdfName <- liftIO $ returnPDF graphSvg graphImg fallsvgFilename fallimageFilename springsvgFilename springimageFilename
    serveFile (asContentType "application/pdf") pdfName

-- | Returns the name of a generated pdf that contains graphImg and timetableImg
-- and deletes all of the img and svg files passed as arguments
returnPDF :: String -> String -> String -> String -> String -> String -> IO String
returnPDF graphSvg graphImg fallTimetableSvg fallTimetableImg springTimetableSvg springTimetableImg = do
    rand <- randomName
    let texName = rand ++ ".tex"
        pdfName = rand ++ ".pdf"
    generateTex [graphImg, fallTimetableImg, springTimetableImg] texName -- generate a temporary TEX file
    createPDF texName                            -- create PDF using TEX and delete the TEX file afterwards
    _ <- removeImage (graphSvg ++ " " ++ graphImg ++ " " ++ fallTimetableSvg ++ " " ++ fallTimetableImg ++ " " ++ springTimetableSvg ++ " " ++ springTimetableImg)
    return pdfName
