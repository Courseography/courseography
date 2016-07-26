{-# LANGUAGE OverloadedStrings #-}

module Response.Export
    (exportGraphResponse, exportTimetableResponse) where

import Control.Monad.IO.Class  (liftIO)
import qualified Data.Map as M
import Happstack.Server
import ImageConversion
import Database.Persist.Sql (toSqlKey)
import Data.Int(Int64)

-- | Serves a pdf file of a graph image
exportGraphResponse :: ServerPart Response
exportGraphResponse = do
    req <- askRq
    let cookies = M.fromList $ rqCookies req
        gId = maybe "1" cookieValue (M.lookup "active-graph" cookies)
        graphKey = read gId :: Int64
    (svgFilename, imageFilename) <- liftIO $ getGraphImage (toSqlKey graphKey) (M.map cookieValue cookies)
    pdfName <- liftIO $ returnPDF svgFilename imageFilename
    serveFile (asContentType "application/pdf") pdfName

-- | Serves a pdf file of a timetable requested by user
-- Note: Not completely working yet! Sending ByteString I think
exportTimetableResponse :: String -> String -> ServerPart Response
exportTimetableResponse course session = do
    (svgFilename, imageFilename) <- liftIO $ getTimetableImage course session
    pdfName <- liftIO $ returnPDF svgFilename imageFilename
    serveFile (asContentType "application/pdf") pdfName

-- | Returns the name of a new pdf created from an image with imageFilename
-- Also deletes some additional files created in the process
returnPDF :: String -> String -> IO String
returnPDF svgFilename imageFilename = do
    let imgName = take (length imageFilename - 4) imageFilename
        pdfName = imgName ++ ".pdf"
    createPDF "export" imgName
    removeImage svgFilename
    removeImage (imgName ++ ".aux")
    removeImage (imgName ++ ".log")
    return $ pdfName