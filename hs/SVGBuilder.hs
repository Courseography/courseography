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
        sqlRects    :: [Entity Rects] <- selectList [] []
        sqlTexts    :: [Entity Texts] <- selectList [] []
        sqlPaths    :: [Entity Paths] <- selectList [] []
        sqlEllipses :: [Entity Ellipses] <- selectList [] []

        let texts      = map (buildText . entityVal) sqlTexts
        let paths      = buildPaths 0 $ (map entityVal) sqlPaths
        let regions    = filter pathIsRegion paths
        let edges      = filter (\x -> not $ pathIsRegion x) paths
        let rects      = map (buildRect texts . entityVal) sqlRects
        --let processedRects = processRects paths rects
        let processedEdges = map (processPath rects) edges
        let processedRects = map (processRect processedEdges) rects

        let rectXml    = map convertRectToXML processedRects

        let textXml    = map (convertTextToXML . buildText . entityVal) sqlTexts
        let edgeXml    = map convertPathToXML processedEdges
        let regionXml  = createRegionXML 0 regions
        let ellipseXml = createEllipseXML 0 texts sqlEllipses
        liftIO $ writeFile  "Testfile.svg" svgHeader
        liftIO $ appendFile "Testfile.svg" regionXml
        liftIO $ appendFile "Testfile.svg" "<g style=\"stroke:#000000\">"
        liftIO $ appendFile "Testfile.svg" $ unwords edgeXml 
        liftIO $ appendFile "Testfile.svg" "</g>"
        liftIO $ appendFile "Testfile.svg" $ unwords rectXml
        liftIO $ appendFile "Testfile.svg"  ellipseXml
        liftIO $ appendFile "Testfile.svg" svgFooter

processRect :: [Path] -> Rect -> Rect
processRect edges rect = do
    let id_ = rectId rect
    let inEdges = map pathId $ filter (\x -> target x == id_) edges
    let outEdges = map pathId $ filter (\x -> source x == id_) edges
    Rect id_
         (width rect)
         (height rect)
         (xPos rect)
         (yPos rect)
         (rectFill rect)
         (rectStroke rect)
         (rectFillOpacity rect)
         (rectIsHybrid rect)
         (rectText rect)
         inEdges
         outEdges

-- | Builds a Path from a database entry in the paths table.
buildPaths :: Int -> [Paths] -> [Path]
buildPaths _ [] = [] 
buildPaths idCounter entities = do
    let entity = head entities
    [Path ("p" ++ show idCounter)
          (map point $ pathsD entity)
          (pathsFill entity)
          (pathsFillOpacity entity)
          (pathsStroke entity)
          (pathsIsRegion entity)
          ""
          ""] ++ (buildPaths (idCounter + 1) $ tail entities)


-- | Builds a Rect from a database entry in the rects table.
buildRect :: [Text] -> Rects -> Rect
buildRect texts entity = do
    let rectTexts = (filter (\x -> (intersects
                            (fromRational (rectsWidth entity))
                            (fromRational (rectsHeight entity))
                            (fromRational (rectsXPos entity))
                            (fromRational (rectsYPos entity))
                            9
                            (fromRational (textXPos x))
                            (fromRational (textYPos x))
                            )) texts)
    let textString = (foldl (++) "" (map textText rectTexts))
    let id_ = (if(rectsIsHybrid entity) then "h" else "") ++ 
              (if isDigit $ head textString then "CSC" else "") ++ dropSlash textString
    Rect id_
         (rectsWidth entity)
         (rectsHeight entity)
         (rectsXPos entity)
         (rectsYPos entity)
         (rectsFill entity)
         (rectsStroke entity)
         (rectsFillOpacity entity)
         (rectsIsHybrid entity)
         rectTexts
         []
         []

processPath :: [Rect] -> Path -> Path
processPath rects edge = 
    do let coords = points edge
       let xStart = fromRational $ fst $ head coords
       let yStart = fromRational $ snd $ head coords
       let xEnd = fromRational $ fst $ last coords
       let yEnd = fromRational $ snd $ last coords
       let sourceNode = getIntersectingNode xStart yStart rects
       let targetNode = getIntersectingNode xEnd yEnd rects
       Path (pathId edge)
            (points edge)
            (pathFill edge)
            (pathFillOpacity edge)
            (pathStroke edge)
            (pathIsRegion edge)
            sourceNode
            targetNode

getIntersectingNode :: Float -> Float -> [Rect] -> String
getIntersectingNode xpos ypos rects = if null
                                      $ filter (intersectsWithPoint xpos ypos) rects
                                      then ""
                                      else rectId $ head $ filter (intersectsWithPoint xpos ypos) rects

intersectsWithPoint :: Float -> Float -> Rect -> Bool
intersectsWithPoint xpos ypos rect = intersects
                            (fromRational $ width rect)
                            (fromRational $ height rect)
                            (fromRational (xPos rect))
                            (fromRational (yPos rect))
                            9
                            xpos
                            ypos

createRegionXML :: Int -> [Path] -> String
createRegionXML _ [] = ""
createRegionXML idCounter paths = (convertRegionToXML (show idCounter) (head paths)) ++ (createRegionXML (idCounter + 1) (tail paths))

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
    "<g id=\"" ++ 
    (rectId rect) ++ 
    "\" class=\"" ++
    (if (rectIsHybrid rect) then "hybrid" else "node") ++
    "\" in-edges=\"" ++ intercalate " " (rectInEdges rect) ++
    "\" out-edges=\"" ++  intercalate " " (rectOutEdges rect) ++ "\"" ++ "><rect rx=\"4\" ry=\"4\"  x=\"" ++ 
    show (fromRational $ xPos rect) ++
    "\" y=\"" ++
    show (fromRational $ yPos rect) ++
    "\" width=\"" ++
    show (fromRational $ width rect) ++
    "\" height=\"" ++
    show (fromRational $ height rect) ++
    "\" style=\"fill:" ++
    (rectFill rect) ++
    ";stroke:#000000" ++ 
    --";stroke:" ++ (rectStroke rect) ++
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
convertPathToXML :: Path -> String
convertPathToXML path = 
    "<path id=\"" ++ (pathId path) ++ "\" class=\"path\" style=\"" ++
    "fill:" ++
    (pathFill path) ++ 
    ";fill-opacity:" ++ (pathFillOpacity path) ++ ";\" d=\"M " ++
    buildPathString (points path) ++
    "\" marker-end=\"url(#arrow)\" " ++
    "source-node=\"" ++ (source path) ++ "\" target-node=\"" ++ (target path) ++ "\"/>"

-- | Converts a `Path` to XML.
convertRegionToXML :: String -> Path -> String
convertRegionToXML id_ path = 
    "<path id=\"region" ++ id_ ++ "\" class=\"region\" style=\"" ++
    "fill:" ++
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