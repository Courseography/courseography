{-# LANGUAGE OverloadedStrings #-}

module Response.Export 
    (pdfResponse) where

import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Base64.Lazy as BEnc
import ImageConversion
<<<<<<< Updated upstream
<<<<<<< Updated upstream
import Svg.Generator
import TimetableImageCreator (renderTable)
import qualified Data.Map as M
import System.Random
import Database.Tables (GraphId)
import Database.Persist.Sql (toSqlKey)
import Data.Int(Int64)
=======
import TimetableImageCreator (renderTable)
import System.Random
>>>>>>> Stashed changes
=======
import TimetableImageCreator (renderTable)
import System.Random
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
<<<<<<< Updated upstream
    compileTexToPdf texFilename
    pdfData <- BS.readFile texFilename
    removeImage svgFilename
    removeImage imageFilename
    removeImage pdfFilename
    removeImage texFilename
=======
=======
>>>>>>> Stashed changes
    _ <- compileTexToPdf texFilename
    pdfData <- BS.readFile texFilename
    _ <- removeImage svgFilename
    _ <- removeImage imageFilename
    _ <- removeImage pdfFilename
    _ <- removeImage texFilename
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
    let encodedData = BEnc.encode pdfData
    return $ toResponse encodedData
 

