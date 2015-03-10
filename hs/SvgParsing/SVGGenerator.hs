{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables #-}
module SvgParsing.SVGGenerator where

import SvgParsing.SVGTypes
import Database.Tables
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Data.Char
import Data.Conduit
import Data.List.Split
import Data.List hiding (map, filter)
import Database.JsonParser
import SvgParsing.ParserUtil
import MakeElements
import Data.Maybe
import qualified Data.Text as T
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import SvgParsing.SVGBuilder
import Text.Blaze.Internal (stringValue)
import Text.Blaze (toMarkup)
import Css.Constants
import qualified Data.Map as M

-- | A list of tuples that contain disciplines (areas), fill values, and courses
--- that are in the areas.
areaMap :: [(String, T.Text, [String])]
areaMap = [("theory", theoryDark, ["csc165", "csc236", "csc240", "csc263", "csc265",
                                   "csc310", "csc324", "csc373", "csc438", "csc448",
                                   "csc463"]),
           ("core", coreDark, ["calc1", "sta1", "sta2", "lin1", "csc108", "csc148", "csc104", "csc120", "csc490",
                               "csc491", "csc494", "csc495"]),
           ("se", seDark, ["csc207", "csc301", "csc302", "csc410", "csc465"]),
           ("systems", systemsDark, ["csc209", "csc258", "csc358", "csc369", "csc372",
                                     "csc458", "csc469", "csc488", "ece385", "ece489"]),
           ("hci", hciDark, ["csc200", "csc300",  "csc318", "csc404", "csc428",
                             "csc454"]),
           ("graphics", graphicsDark,["csc320", "csc418", "csc420"]),
           ("num", numDark, ["csc336", "csc436", "csc446", "csc456"]),
           ("ai", aiDark, ["csc321", "csc384", "csc401", "csc411", "csc412",
                           "csc485", "csc486"]),
           ("dbweb", dbwebDark , ["csc309", "csc343", "csc443"])]

-- | The style for Text elements of hybrids.
hybridTextStyle :: String
hybridTextStyle = "font-size:7.5pt;fill:white;"

-- | The style for Text elements of ellipses.
ellipseTextStyle :: String
ellipseTextStyle = "font-size:7.5pt;"

-- | The style for Text elements of ellipses.
regionTextStyle :: String
regionTextStyle = "font-size:9pt;"

-- | Gets the first element of a tuple with length 3.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Gets the second element of a tuple with length 3.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Gets the third element of a tuple with length 3.
thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c

-- | Gets a tuple from areaMap where id_ is in the list of courses for that tuple.
getTuple :: String -> (String, T.Text, [String])
getTuple id_ = head $ filter (\x -> id_ `elem` thrd3 x) areaMap ++ [("", "grey", [])]

-- | Gets an area from areaMap where id_ is in the list of courses for the corresponding tuple.
getArea :: String -> String
getArea id_ = fst3 $ getTuple id_

-- | Gets the fill from areaMap where id_ is in the list of courses for the corresponding tuple.
getFill :: String -> String
getFill id_ = T.unpack $ snd3 $ getTuple id_

-- | Builds an SVG document.
makeSVGDoc :: M.Map String String -> [Shape] -> [Shape] -> [Path] -> [Path] -> [Text] -> S.Svg
makeSVGDoc courseMap rects ellipses edges regions regionTexts =
    S.docTypeSvg ! A.width "1052.3622"
                 ! A.height "744.09448"
                 ! S.customAttribute "xmlns:svg" "http://www.w3.org/2000/svg"
                 ! S.customAttribute "xmlns:dc" "http://purl.org/dc/elements/1.1/"
                 ! S.customAttribute "xmlns:cc" "http://creativecommons.org/ns#"
                 ! S.customAttribute "xmlns:rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 ! A.version "1.1" $ do
                      makeSVGDefs
                      S.g ! A.id_ "regions" $
                          concatSVG $ map convertRegionToSVG
                                          regions
                      S.g ! A.id_ "nodes"
                          ! A.style "stroke:#000000;" $
                              concatSVG $ map (convertRectToSVG courseMap)
                                              rects
                      S.g ! A.id_ "bools" $
                          concatSVG $ map convertEllipseToSVG
                                          ellipses
                      S.g ! A.id_ "edges"
                          ! A.style "stroke:#000000" $
                              concatSVG $ map convertEdgeToSVG
                                              edges
                      S.g ! A.id_ "region-labels" $
                          concatSVG $ map (convertTextToSVG False False True)
                                          regionTexts

-- | Builds the SVG defs.
makeSVGDefs :: S.Svg
makeSVGDefs =
    S.defs $
    S.marker ! A.id_ "arrow"
             ! A.viewbox "0 0 10 10"
             ! A.refx "1"
             ! A.refy "5"
             ! A.markerunits "strokeWidth"
             ! A.orient "auto"
             ! A.markerwidth "7"
             ! A.markerheight "7" $
                 S.polyline ! A.points "0,1 10,5 0,9"
                            ! A.fill "black"

-- | Builds an SVG document.
buildSVG :: M.Map String String -> String -> IO ()
buildSVG courseMap filename =
    runSqlite dbStr $ do
        sqlRects    :: [Entity Rects]    <- selectList [] []
        sqlTexts    :: [Entity Texts]    <- selectList [] []
        sqlPaths    :: [Entity Paths]    <- selectList [] []
        sqlEllipses :: [Entity Ellipses] <- selectList [] []
        let courseStyleMap = M.map convertSelectionToStyle courseMap
            texts       = map (buildText . entityVal) sqlTexts
            rects       = map (buildRect texts . entityVal) sqlRects
            ellipses    = buildEllipses texts 0 $ map entityVal sqlEllipses
            paths       = zipWith (buildPath rects ellipses) (map entityVal sqlPaths) [1..length sqlPaths]
            regions     = filter pathIsRegion paths
            edges       = filter (not . pathIsRegion) paths
            regionTexts = filter (not . intersectsWithShape (rects ++ ellipses)) texts
            stringSVG = renderSvg $ makeSVGDoc courseStyleMap rects ellipses edges regions regionTexts
        liftIO $ writeFile filename stringSVG

-- | Determines if a text intersects with a shape.
intersectsWithShape :: [Shape] -> Text -> Bool
intersectsWithShape shapes text =
    not $ null (filter (intersectsWithPoint (fromRational $ textXPos text) (fromRational $ textYPos text)) shapes)

-- | Converts a `Rect` to SVG.
convertRectToSVG :: M.Map String String -> Shape -> S.Svg
convertRectToSVG courseMap rect
    | shapeFill rect == "none" = S.rect
    | otherwise =
        S.g ! A.id_ (stringValue $ shapeId rect)
            ! A.class_ (if shapeIsHybrid rect then "hybrid" else "node")
            ! S.customAttribute "data-group" (stringValue (getArea (shapeId rect)))
            ! S.customAttribute "text-rendering" "geometricPrecision"
            ! S.customAttribute "shape-rendering" "geometricPrecision"
            ! A.style (stringValue (
                       if not (shapeIsHybrid rect)
                       then case M.lookup (shapeId rect) courseMap of
                                 Just style -> style
                                 Nothing    -> ""
                       else "")) $
            do S.rect ! A.rx "4"
                      ! A.ry "4"
                      ! A.x (stringValue $ show $ fromRational $ shapeXPos rect)
                      ! A.y (stringValue $ show $ fromRational $ shapeYPos rect)
                      ! A.width (stringValue $ show $ fromRational $ shapeWidth rect)
                      ! A.height (stringValue $ show $ fromRational $ shapeHeight rect)
                      ! A.style (stringValue $ "fill:" ++ getFill (shapeId rect) ++ ";")
               concatSVG $ map (convertTextToSVG (shapeIsHybrid rect) False False) (shapeText rect)

convertSelectionToStyle :: String -> String
convertSelectionToStyle courseStatus =
    if isSelected courseStatus
    then "stroke-width:4;"
    else "opacity:0.5;stroke-dasharray:8,5;"

isSelected :: String -> Bool
isSelected courseStatus =
    isPrefixOf "active" courseStatus ||
    isPrefixOf "overridden" courseStatus

-- | Converts a `Text` to SVG.
convertTextToSVG :: Bool -> Bool -> Bool -> Text -> S.Svg
convertTextToSVG isHybrid isBool isRegion text =
    S.text_ ! A.x (stringValue $ show $ fromRational $ textXPos text)
            ! A.y (stringValue $ show $ fromRational $ textYPos text)
            ! A.style (stringValue $
                       (getTextStyle isHybrid isBool isRegion) ++
                       "font-family:sans-serif;stroke:none;")
            $ toMarkup $ textText text

-- | Converts a `Path` to SVG.
convertEdgeToSVG :: Path -> S.Svg
convertEdgeToSVG path =
    S.path ! A.id_ (stringValue $ "path" ++ pathId path)
           ! A.class_ "path"
           ! A.d (stringValue $ 'M' : buildPathString (points path))
           ! A.markerEnd "url(#arrow)"
           ! S.customAttribute "source-node" (stringValue $ source path)
           ! S.customAttribute "target-node" (stringValue $ target path)
           ! A.style (stringValue $ "fill:" ++
                      pathFill path ++
                      ";fill-opacity:1;")

-- | Converts a `Path` to SVG.
convertRegionToSVG :: Path -> S.Svg
convertRegionToSVG path =
    S.path ! A.id_ (stringValue $ "region" ++ pathId path)
           ! A.class_ "region"
           ! A.d (stringValue $ 'M' : buildPathString (points path))
           ! A.style (stringValue $ "fill:" ++
                      pathFill path ++
                      ";opacity:0.7;fill-opacity:0.58;")

-- | Converts an `Ellipse` to SVG.
convertEllipseToSVG :: Shape -> S.Svg
convertEllipseToSVG ellipse =
    S.g ! A.id_ (stringValue (shapeId ellipse))
        ! A.class_ "bool" $ do
            S.ellipse ! A.cx (stringValue $ show $ fromRational $ shapeXPos ellipse)
                      ! A.cy (stringValue $ show $ fromRational $ shapeYPos ellipse)
                      ! A.rx (stringValue $ show $ fromRational $ shapeWidth ellipse / 2)
                      ! A.ry (stringValue $ show $ fromRational $ shapeHeight ellipse / 2)
                      ! A.style "stroke:#000000;fill:none;"
            concatSVG $ map (convertTextToSVG False True False) (shapeText ellipse)

getTextStyle :: Bool -> Bool -> Bool -> String
getTextStyle True _ _ = hybridTextStyle
getTextStyle _ True _ = ellipseTextStyle
getTextStyle _ _ True = regionTextStyle
getTextStyle _ _ _     = ""
