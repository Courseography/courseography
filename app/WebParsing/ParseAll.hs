module WebParsing.ParseAll
     (parseAll) where

import WebParsing.ArtSciParser
import WebParsing.TimeTableParser
import WebParsing.PostParser

parseAll :: IO ()
parseAll = do
    parseArtSci
    --parseUTSC
    parseTT
    parsePosts
