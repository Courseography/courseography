{-# LANGUAGE OverloadedStrings #-}
module WebParsing.ParseAll (parseAll) where

import WebParsing.UTSCParser
import WebParsing.ArtSciParser

parseAll :: IO()
parseAll = do
	parseArtSci
	parseUTSC

