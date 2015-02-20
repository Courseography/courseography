{-# LANGUAGE OverloadedStrings #-}
module ParseAll (parseAll) where

import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as B
import Data.List.Utils
import Data.Maybe
import Tables
import ParsingHelp
import UTSCParser
import ArtSciParser

parseAll :: IO()
parseAll = do
	parseArtSci
	parseUTSC
	
