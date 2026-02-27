{-|
    Module      : Util.Helpers
    Description : Contains general-use helper functions.
-}
module Util.Helpers
    (safeHead, readImageData, returnImageData) where

import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Base64 as BEnc (encode)
import Happstack.Server (Response, toResponse)
import System.Directory (removeFile)

-- | Given a list and a default value, returns the head of the list, or the default value
-- if the list is empty.
safeHead :: a -> [a] -> a
safeHead listHead [] = listHead
safeHead _ (listHead:_) = listHead

-- | Reads the data in an image file and returns the data as a response.
readImageData :: String -> IO Response
readImageData imageFileName = do
    imageData <- BS.readFile imageFileName
    let encodedData = BEnc.encode imageData
    return $ toResponse encodedData

-- | Creates and converts an SVG file to an image file, deletes them both and
-- returns the image data as a response.
returnImageData :: String -> String -> IO Response
returnImageData svgFilename imageFilename = do
    response <- readImageData imageFilename
    _ <- removeFile imageFilename
    _ <- removeFile svgFilename
    return response
