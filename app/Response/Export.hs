module Response.Export
    (exportGraphResponse) where

import Control.Monad.IO.Class  (liftIO)
import Happstack.Server
import GetImages
import ImageConversion (removeImage)
import PdfGenerator
import LatexGenerator

-- | Serves a pdf file that includes a graph and timetable information from 
-- selected course sessions
exportGraphResponse :: String -> String -> ServerPart Response
exportGraphResponse courses session = do
    req <- askRq
    (graphSvg, graphImg) <- liftIO $ getActiveGraphImage req                    -- create image of active graph
    (timetableSvg, timetableImg) <- liftIO $ getTimetableImage courses session  -- create timetable image from course and session 
    pdfName <- liftIO $ returnPDF graphSvg graphImg timetableSvg timetableImg   -- create pdf with both graph and timetable
    serveFile (asContentType "application/pdf") pdfName

-- | Returns the name of a generated pdf that contains graphImg and timetableImg
-- and deletes all of the img and svg files passed as arguments
returnPDF :: String -> String -> String -> String -> IO String
returnPDF graphSvg graphImg timetableSvg timetableImg = do
    rand <- randomName
    let texName = rand ++ ".tex"
        pdfName = rand ++ ".pdf"
    generateTex [graphImg, timetableImg] texName -- generate a temporary TEX file
    createPDF texName                            -- create PDF using TEX and delete the TEX file afterwards
    removeImage (graphSvg ++ " " ++ graphImg ++ " " ++ timetableSvg ++ " " ++ timetableImg)
    return $ (pdfName)
