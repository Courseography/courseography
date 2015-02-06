{-# LANGUAGE OverloadedStrings #-}

module ImageResponse where

import Data.List
import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Base64.Lazy as B

import ConvertSVGToPNG
imageResponse :: ServerPart Response
imageResponse = liftIO $ getImage

-- | Creates an image, and returns the base64 representation of that image.
getImage :: IO Response
getImage = do
	liftIO $ createPNGFile "INSERT_ID-graph.png"
	imageData <- BS.readFile "INSERT_ID-graph.png"
	liftIO $ removePNG "INSERT_ID-graph.png"
	let encodedData = B.encode imageData
	return $ toResponse encodedData
