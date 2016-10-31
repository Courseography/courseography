module WebParsing.ParseAll
     (parseAll) where

import WebParsing.ArtSciParser
import WebParsing.UtsgJsonParser (getAllCourses)

parseAll :: IO ()
parseAll = do
    parseArtSci
    --parseUTSC
    getAllCourses
