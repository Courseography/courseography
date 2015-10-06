{-# LANGUAGE OverloadedStrings #-}

module Response.Export 
    (pdfResponse) where

import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64.Lazy as BEnc
import ImageConversion
import TimetableImageCreator (renderTable)
import System.Random
import Latex

-- | Returns a PDF containing the image of the timetable
-- requested by the user.
pdfResponse :: String -> String -> ServerPart Response
pdfResponse courses session =
    liftIO $ getPdf courses session

getPdf :: String -> String -> IO Response
getPdf courses session = do
    gen <- newStdGen
    let (rand, _) = next gen
        svgFilename = (show rand ++ ".svg")
        imageFilename = (show rand ++ ".png")
        texFilename = (show rand ++ ".tex")
        pdfFilename = (drop 4 texFilename) ++ ".pdf"
    renderTable svgFilename courses session
    returnPdfData svgFilename imageFilename pdfFilename texFilename

returnPdfData :: String -> String -> String -> String -> IO Response
returnPdfData svgFilename imageFilename pdfFilename texFilename = do
    createImageFile svgFilename imageFilename
    compileTex texFilename imageFilename
    _ <- compileTexToPdf texFilename
    pdfData <- BS.readFile texFilename
    _ <- removeImage svgFilename
    _ <- removeImage imageFilename
    _ <- removeImage pdfFilename
    _ <- removeImage texFilename
    let encodedData = BEnc.encode pdfData
    return $ toResponse encodedData
 

