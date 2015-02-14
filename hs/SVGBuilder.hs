{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module SVGBuilder where

import SVGTypes
import Tables
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Data.Conduit
import Data.List
import JsonParser

-- | The SVG tag for an SVG document, along with an opening 'g' tag.
svgHeader :: String
svgHeader = "<svg" ++
   " xmlns:dc=\"http://purl.org/dc/elements/1.1/\"" ++
   " xmlns:cc=\"http://creativecommons.org/ns#\"" ++
   " xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"" ++
   " xmlns:svg=\"http://www.w3.org/2000/svg\"" ++
   " xmlns=\"http://www.w3.org/2000/svg\"" ++
   " xmlns:xlink=\"http://www.w3.org/1999/xlink\"" ++
   " xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"" ++
   " xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"" ++
   " width=\"1052.3622\"" ++
   " height=\"744.09448\"" ++
   " id=\"svg2\"" ++
   " version=\"1.1\"" ++
   " sodipodi:docname=\"graph_regions.svg\"><g>"

-- | A closing 'g' tag followed by a closing 'svg' tag.
svgFooter :: String
svgFooter = "</g></svg>"

-- | Builds an SVG document.
buildSVG :: IO ()
buildSVG = 
    runSqlite dbStr $ do
        sqlRects :: [Entity Rects] <- selectList [] []
        sqlTexts :: [Entity Texts] <- selectList [] []
        sqlPaths :: [Entity Paths] <- selectList [] []
        let rectXml = map (convertRectToXML . buildRect . entityVal) sqlRects
        let textXml = map (convertTextToXML . buildText . entityVal) sqlTexts
        let pathXml = map (convertPathToXML . buildPath . entityVal) sqlPaths
        liftIO $ writeFile "Testfile.svg" svgHeader
        liftIO $ appendFile "Testfile.svg" $ unwords pathXml
        liftIO $ appendFile "Testfile.svg" $ unwords rectXml
        liftIO $ appendFile "Testfile.svg" $ unwords textXml
        liftIO $ appendFile "Testfile.svg" svgFooter

-- | Prints the database table 'rects'.
printDB :: IO ()
printDB = runSqlite dbStr $ do
              let sql = "SELECT * FROM rects"
              rawQuery sql [] $$ CL.mapM_ (liftIO . print)

-- | Converts a `Rect` to XML. 
convertRectToXML :: Rect -> String
convertRectToXML rect = 
    "<rect rx=\"4\" ry=\"4\" x=\"" ++ 
    show (fromRational $ xPos rect) ++
    "\" y=\"" ++
    show (fromRational $ yPos rect) ++
    "\" width=\"" ++
    show (fromRational $ width rect) ++
    "\" height=\"" ++
    show (fromRational $ height rect) ++
    "\" style=\"fill:" ++
    (rectFill rect) ++
    ";stroke:" ++ (rectStroke rect) ++ ";\"/>"

-- | Converts a `Text` to XML.
convertTextToXML :: Text -> String
convertTextToXML text = 
    "<text xml:space=\"preserve\" x=\"" ++ 
    (show $ fromRational $ textXPos text) ++
    "\" y=\"" ++
    (show $ fromRational $ textYPos text) ++
    "\" style=\"font-size:" ++
    (textFontSize text) ++
    "\">" ++
    (textText text) ++
    "</text>"

-- | Converts a `Path` to XML.
convertPathToXML :: Path -> String
convertPathToXML path = 
    "<path style=\"stroke:#000000;fill:" ++ (if null (pathFill path)
                                            then "none"
                                            else (pathFill path)) ++ ";\" d=\"M " ++
    buildPathString (points path) ++
    "\"/>"

-- | Builds a Rect from a database entry in the rects table.
buildRect :: Rects -> Rect
buildRect entity = 
    Rect (rectsWidth entity)
         (rectsHeight entity)
         (rectsXPos entity)
         (rectsYPos entity)
         (rectsFill entity)
         (rectsStroke entity)

-- | Builds a Text from a database entry in the texts table.
buildText :: Texts -> Text
buildText entity = 
    Text (textsXPos entity)
         (textsYPos entity)
         (textsText entity)
         (textsFontSize entity)

-- | Builds a Path from a database entry in the paths table.
buildPath :: Paths -> Path
buildPath entity = 
    Path (map point $ pathsD entity)
         (pathsFill entity)

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [(Rational, Rational)] -> String
buildPathString d = intercalate " " $ map (joinPathTuple . convertRationalTupToString) d

-- | Joins two String values in a tuple with a comma.
joinPathTuple :: (String, String) -> String
joinPathTuple tup = fst tup ++ "," ++ snd tup

-- | Converts a tuple of Rationals to a tuple of String.
convertRationalTupToString :: (Rational, Rational) -> (String, String)
convertRationalTupToString tup = (show $ fromRational (fst tup), show $ fromRational (snd tup))
