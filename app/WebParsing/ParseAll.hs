module WebParsing.ParseAll
     (parseAll) where

import WebParsing.ArtSciParser (parseArtSci)
import WebParsing.UtsgJsonParser (getAllCourses)

parseAll :: IO ()
parseAll = do
    parseArtSci
    getAllCourses
