{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module SVGBuilder where

import SVGTypes
import Tables
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Data.Char
import Data.Conduit
import Data.List.Split
import Data.List
import JsonParser
import ParserUtil

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
   " sodipodi:docname=\"graph_regions.svg\"><defs>" ++
   "     <marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"1\" refY=\"5\" markerUnits=\"strokeWidth\" orient=\"auto\" markerWidth=\"7\" markerHeight=\"7\">" ++
   "       <polyline points=\"0,1 10,5 0,9\" fill=\"black\"></polyline>" ++
   "     </marker>" ++
   "   </defs><g>"

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
        sqlEllipses :: [Entity Ellipses] <- selectList [] []
        let texts = map (buildText . entityVal) sqlTexts
        let paths = map (buildPath . entityVal) sqlPaths
        let regions = filter pathIsRegion paths
        let edges = filter (\x -> not $ pathIsRegion x) paths
        let rectXml = map (convertRectToXML . buildRect texts . entityVal) sqlRects
        let textXml = map (convertTextToXML . buildText . entityVal) sqlTexts
        let edgeXml = createPathXML 0 edges
        let regionXml = createPathXML 0 regions
        let ellipseXml = createEllipseXML 0 texts sqlEllipses
        liftIO $ writeFile "Testfile.svg" svgHeader
        liftIO $ appendFile "Testfile.svg" regionXml
        liftIO $ appendFile "Testfile.svg" "<g style=\"stroke:#000000\">"
        liftIO $ appendFile "Testfile.svg" edgeXml 
        liftIO $ appendFile "Testfile.svg" "</g>"
        liftIO $ appendFile "Testfile.svg" $ unwords rectXml
        liftIO $ appendFile "Testfile.svg"  ellipseXml
        liftIO $ appendFile "Testfile.svg" svgFooter

createRegionXML :: Int -> [Path] -> String
createRegionXML _ [] = ""
createRegionXML idCounter paths = (convertRegionToXML (show idCounter) (head paths)) ++ (createRegionXML (idCounter + 1) (tail paths))

createPathXML :: Int -> [Path] -> String
createPathXML _ [] = ""
createPathXML idCounter paths = (convertPathToXML (show idCounter) (head paths)) ++ (createPathXML (idCounter + 1) (tail paths))

createEllipseXML :: Int -> [Text] -> [Entity Ellipses] -> String
createEllipseXML _ _ [] = ""
createEllipseXML idCounter texts ellipses = (((convertEllipseToXML (show idCounter)) .
                                 buildEllipse texts .
                                 entityVal) (head ellipses)) ++
                                 (createEllipseXML (idCounter + 1) texts (tail ellipses))

-- | Prints the database table 'rects'.
printDB :: IO ()
printDB = runSqlite dbStr $ do
              let sql = "SELECT * FROM rects"
              rawQuery sql [] $$ CL.mapM_ (liftIO . print)

-- | Converts a `Rect` to XML. 
convertRectToXML :: Rect -> String
convertRectToXML rect = 
    if (rectFill rect) == "none" then "" else
    "<g id=\"" ++ (if (rectIsHybrid rect) then "h" else "") ++
    (if (isDigit $ head (foldl (\x y -> x ++ y) "" (map textText (rectText rect)))) then "CSC" else "") ++
    dropSlash (foldl (\x y -> x ++ y) "" (map textText (rectText rect))) ++ 
    "\" class=\"" ++
    (if (rectIsHybrid rect) then "hybrid" else "node") ++
    "\"><rect rx=\"4\" ry=\"4\"  x=\"" ++ 
    show (fromRational $ xPos rect) ++
    "\" y=\"" ++
    show (fromRational $ yPos rect) ++
    "\" width=\"" ++
    show (fromRational $ width rect) ++
    "\" height=\"" ++
    show (fromRational $ height rect) ++
    "\" style=\"fill:" ++
    (rectFill rect) ++
    ";stroke:" ++ (rectStroke rect) ++
    ";fill-opacity:" ++ (rectFillOpacity rect) ++ ";\"/>"
    ++ (unwords (map convertTextToXML (rectText rect))) ++
    "</g>"

-- | Converts a `Text` to XML.
convertTextToXML :: Text -> String
convertTextToXML text = 
    "<text xml:space=\"preserve\" x=\"" ++ 
    (show $ fromRational $ textXPos text) ++
    "\" y=\"" ++
    (show $ fromRational $ textYPos text) ++
    "\" style=\"font-size:" ++
    (textFontSize text) ++
    ";font-weight:" ++ 
    (textFontWeight text) ++ 
    ";font-family:" ++
    (textFontFamily text) ++
    "\">" ++
    (textText text) ++
    "</text>"

-- | Converts a `Path` to XML.
convertPathToXML :: String -> Path -> String
convertPathToXML id_ path = 
    "<path id=\"p" ++ id_ ++ "\" class=\"path\" style=\"stroke-dasharray:none;" ++
    ";fill:" ++
    (pathFill path) ++ 
    ";fill-opacity:" ++ (pathFillOpacity path) ++ ";\" d=\"M " ++
    buildPathString (points path) ++
    "\" marker-end=\"url(#arrow)\"/>"

-- | Converts a `Path` to XML.
convertRegionToXML :: String -> Path -> String
convertRegionToXML id_ path = 
    "<path id=\"region" ++ id_ ++ "\" class=\"region\" style=\"stroke-dasharray:none;" ++
    ";fill:" ++
    (pathFill path) ++ 
    ";fill-opacity:" ++ (pathFillOpacity path) ++ ";\" d=\"M " ++
    buildPathString (points path) ++
    "\"/>"

-- | Converts an `Ellipse` to XML.
convertEllipseToXML :: String -> Ellipse -> String
convertEllipseToXML id_ ellipse = 
    "<g id=\"bool" ++ id_ ++ "\" class=\"bool\"><ellipse cx=\"" ++ 
    show (fromRational $ ellipseXPos ellipse) ++
    "\" cy=\"" ++
    show (fromRational $ ellipseYPos ellipse) ++
    "\" rx=\"" ++ 
    show (fromRational $ ellipseRx ellipse) ++
    "\" ry=\"" ++
    show (fromRational $ ellipseRy ellipse) ++
    "\" style=\"stroke:#000000;fill:none" ++
    "\"/>"
    ++ (unwords (map convertTextToXML (ellipseText ellipse))) ++
    "</g>"

-- | Builds a Rect from a database entry in the rects table.
buildRect :: [Text] -> Rects -> Rect
buildRect texts entity = 
    Rect (rectsWidth entity)
         (rectsHeight entity)
         (rectsXPos entity)
         (rectsYPos entity)
         (rectsFill entity)
         (rectsStroke entity)
         (rectsFillOpacity entity)
         (rectsIsHybrid entity)
         (filter (\x -> (intersects
                            (fromRational (rectsWidth entity))
                            (fromRational (rectsHeight entity))
                            (fromRational (rectsXPos entity))
                            (fromRational (rectsYPos entity))
                            9
                            (fromRational (textXPos x))
                            (fromRational (textYPos x))
                            )) texts)

-- | Builds a Text from a database entry in the texts table.
buildText :: Texts -> Text
buildText entity = 
    Text (textsXPos entity)
         (textsYPos entity)
         (textsText entity)
         (textsFontSize entity)
         (textsFontWeight entity)
         (textsFontFamily entity)

-- | Builds a Path from a database entry in the paths table.
buildPath :: Paths -> Path
buildPath entity = 
    Path (map point $ pathsD entity)
         (pathsFill entity)
         (pathsFillOpacity entity)
         (pathsStroke entity)
         (pathsIsRegion entity)

-- | Builds a Path from a database entry in the paths table.
buildEllipse :: [Text] -> Ellipses -> Ellipse
buildEllipse texts entity = 
    Ellipse (ellipsesXPos entity)
            (ellipsesYPos entity)
            (ellipsesRx entity)
            (ellipsesRy entity)
            (ellipsesStroke entity)
            (filter (\x -> (intersects
                            5
                            5
                            (fromRational (ellipsesXPos entity))
                            (fromRational (ellipsesYPos entity))
                            9
                            (fromRational (textXPos x))
                            (fromRational (textYPos x))
                            )) texts)

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [(Rational, Rational)] -> String
buildPathString d = intercalate " " $ map (joinPathTuple . convertRationalTupToString) d

-- | Joins two String values in a tuple with a comma.
joinPathTuple :: (String, String) -> String
joinPathTuple tup = fst tup ++ "," ++ snd tup

-- | Converts a tuple of Rationals to a tuple of String.
convertRationalTupToString :: (Rational, Rational) -> (String, String)
convertRationalTupToString tup = (show $ fromRational (fst tup), show $ fromRational (snd tup))


dropSlash :: String -> String
dropSlash str = head $ splitOn "/" str