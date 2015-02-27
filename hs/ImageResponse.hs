{-# LANGUAGE OverloadedStrings #-}

module ImageResponse where

import Data.List
import Happstack.Server
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as B
import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Base64.Lazy as BEnc
import ConvertSVGToPNG
import Data.List.Split
import SVGGenerator
import ParserUtil

-- | Returns an image requested by the user.
imageResponse :: ServerPart Response
imageResponse = do req <- askRq
                   let cookie = getHeader "cookie" $ rqHeaders req
                   liftIO $ getImage cookie

-- | Creates an image, and returns the base64 representation of that image.
getImage :: Maybe B.ByteString -> IO Response
getImage (Just cookie) = do
	let courseMap = map (\x -> (dropSlash $ replaceEscapedQuotation $ fst x, snd x)) $ parseCookies $ show cookie
	buildSVG courseMap "Testfile2.svg"
	liftIO $ print courseMap
	liftIO $ createPNGFile "INSERT_ID-graph.png"
	imageData <- BS.readFile "INSERT_ID-graph.png"
	liftIO $ removePNG "INSERT_ID-graph.png"
	let encodedData = BEnc.encode imageData
	return $ toResponse encodedData
-- TODO: add Nothing case.

-- | Parses the cookie string into a series of tuples of course code and
-- node value pairs, where node value is either overridden, active, inactive
-- or takeable.
parseCookies :: String -> [(String, String)]
parseCookies cookies = map (\x -> let y = splitOn "=" x in (head y, last y)) $ splitOn " " cookies
