module WebParsing.ParseAll
     (parseAll) where

import WebParsing.ArtSciParser (parseArtSci, parseBuildings)
import WebParsing.UtsgJsonParser (getAllCourses)

parseAll :: IO ()
parseAll = do
    parseArtSci
    getAllCourses
    parseBuildings
