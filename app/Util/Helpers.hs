{-|
    Module      : Util.Helpers
    Description : Contains general-use helper functions.
-}
module Util.Helpers
    (safeHead, returnImageData) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BEnc
import Happstack.Server
import System.Directory (removeFile)

-- | Given a list and a default value, returns the head of the list, or the default value
-- if the list is empty.
safeHead :: a -> [a] -> a
safeHead listHead [] = listHead
safeHead _ (listHead:_) = listHead

-- | Creates and converts an SVG file to an image file, deletes them both and
-- returns the image data as a response.
returnImageData :: String -> String -> IO Response
returnImageData svgFilename imageFilename = do
    imageData <- BS.readFile imageFilename
    _ <- removeFile imageFilename
    _ <- removeFile svgFilename
    let encodedData = BEnc.encode imageData
    return $ toResponse encodedData
