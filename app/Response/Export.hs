module Response.Export
    (exportGraphResponse) where

import Control.Monad.IO.Class  (liftIO)
import Happstack.Server
import GetImages
import ImageConversion (removeImage)
import PdfGenerator

-- | Serves a pdf file that includes a graph and timetable information from 
-- selected course sessions
exportGraphResponse :: String -> String -> ServerPart Response
exportGraphResponse courses session = do
    req <- askRq
    (graphSvg, graphImg) <- liftIO $ getActiveGraphImage req                    -- create image of active graph
    (timetableSvg, timetableImg) <- liftIO $ getTimetableImage courses session  -- create timetable image from course and session 
    pdfName <- liftIO $ returnPDF graphSvg graphImg timetableSvg timetableImg   -- create pdf with both graph and timetable
    serveFile (asContentType "application/pdf") pdfName

-- | Returns the name of a new pdf that contains graphImg and timetableImg
-- and deletes all of the img and svg files passed as arguments
returnPDF :: String -> String -> String -> String -> IO String
returnPDF graphSvg graphImg timetableSvg timetableImg = do
    let pdfName = take (length graphImg - 4) graphImg  -- remove .png extension; pdfName should not have an extension for createPDF
    createPDF "export" pdfName graphImg timetableImg
    removeImage graphSvg
    removeImage graphImg
    removeImage timetableSvg
    removeImage timetableImg
    return $ (pdfName ++ ".pdf")
