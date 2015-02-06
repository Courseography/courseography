{-# LANGUAGE OverloadedStrings #-}

module ImageResponse where
import Data.List
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate
import Scripts
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Conduit.List as CL
import ConvertSVGToPNG
import Data.ByteString.Base64.Lazy as B
import SVGGen

imageResponse :: ServerPart Response
imageResponse = liftIO $ getImage

getImage :: IO Response
getImage = do
	liftIO $ createPNGFile "INSERT_ID-graph.png"
	t <- BS.readFile "INSERT_ID-graph.png"
	liftIO $ removePNG ("INSERT_ID-graph.png")
	let x = B.encode t
	return $ toResponse x
