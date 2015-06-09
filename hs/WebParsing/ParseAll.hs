module WebParsing.ParseAll (parseAll) where

import WebParsing.ArtSciParser
import WebParsing.TimeTableParser

parseAll :: IO ()
parseAll = do
    parseArtSci
    --parseUTSC
    parseTT
