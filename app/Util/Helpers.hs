{-|
    Module      : Util.Helpers
    Description : Contains general-use helper functions.
-}
module Util.Helpers
    (safeHead, readImageData) where

import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Base64 as BEnc (encode)
import Happstack.Server (Response, toResponse)

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
