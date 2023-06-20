module WebParsing.ParseAll
     (parseCalendar, parseTimetable) where

import WebParsing.ArtSciParser (parseArtSci, parseBuildings)
import WebParsing.UtsgJsonParser (getAllCourses)

parseCalendar :: IO ()
parseCalendar = do
    parseArtSci
    parseBuildings

parseTimetable :: IO ()
parseTimetable = do
    getAllCourses
