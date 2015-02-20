{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables #-}
module SVGGenerator where

import SVGTypes
import Tables
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Data.Char
import Data.Conduit
import Data.List.Split
import Data.List hiding (map, filter)
import JsonParser
import ParserUtil
import MakeElements
import Data.Maybe
import qualified Data.Text as T
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import SVGBuilder
import Text.Blaze.Internal (stringValue)
import Text.Blaze (toMarkup)
import CssGen

-- | A list of tuples that contain disciplines (areas), fill values, and courses
--- that are in the areas.
areaMap :: [(String, String, [String])]
areaMap = [("theory", theoryDark, ["CSC165", "CSC236", "CSC240", "CSC263", "CSC265",
                       "CSC310", "CSC324", "CSC373", "CSC438", "CSC448",
                       "CSC463"]),
           ("core", coreDark, ["Calc1", "Sta1", "Sta2", "Lin1", "CSC108", "CSC148", "CSC104", "CSC120", "CSC490",
                               "CSC491", "CSC494", "CSC495"]),
           ("se", seDark, ["CSC207", "CSC301", "CSC302", "CSC410", "CSC465"]),
           ("systems", systemsDark, ["CSC209", "CSC258", "CSC358", "CSC369", "CSC372",
                                     "CSC458", "CSC469", "CSC488", "ECE385", "ECE489"]),
           ("hci", hciDark, ["CSC200", "CSC300",  "CSC318", "CSC404", "CSC428",
                             "CSC454"]),
           ("graphics", graphicsDark,["CSC320", "CSC418", "CSC420"]),
           ("num", numDark, ["CSC336", "CSC436", "CSC446", "CSC456"]),
           ("ai", aiDark, ["CSC321", "CSC384", "CSC401", "CSC411", "CSC412",
                          "CSC485", "CSC486"]),
           ("dbweb", dbwebDark , ["CSC309", "CSC343", "CSC443"])]

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
fst3 (a, b, c) = a

-- | Gets the second element of a tuple with length 3.
snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b

-- | Gets the third element of a tuple with length 3.
thrd3 :: (a, b, c) -> c
thrd3 (a,b,c) = c

-- | Gets a tuple from areaMap where id_ is in the list of courses for that tuple.
getTuple :: String -> (String, String, [String])
getTuple id_ = head $ (filter (\x -> elem id_ (thrd3 x)) areaMap) ++ [("", "grey", [])]

-- | Gets an area from areaMap where id_ is in the list of courses for the corresponding tuple.
getArea :: String -> String
getArea id_ = fst3 $ getTuple id_

-- | Gets the fill from areaMap where id_ is in the list of courses for the corresponding tuple.
getFill :: String -> String
getFill id_ = snd3 $ getTuple id_

-- | Builds an SVG document.
makeSVGDoc :: [Shape] -> [Shape] -> [Path] -> [Path] -> [Text] -> S.Svg
makeSVGDoc rects ellipses edges regions regionTexts =
    S.docTypeSvg ! A.width "1052.3622"
                 ! A.height "744.09448"
                 ! A.version "1.1" $ do
                      makeSVGDefs
                      S.g ! A.id_ "regions" $ concatSVG $ map convertRegionToSVG regions
                      S.g ! A.id_ "nodes" $ concatSVG $ map convertRectToSVG rects
                      S.g ! A.id_ "bools" $ concatSVG $ map convertEllipseToSVG ellipses
                      S.g ! A.id_ "edges" ! A.style "stroke:#000000" $ concatSVG $ map convertEdgeToSVG edges
                      S.g ! A.id_ "region-labels" $ concatSVG $ map (convertTextToSVG False False True) regionTexts

-- | Builds the SVG defs.
makeSVGDefs :: S.Svg
makeSVGDefs = S.defs $ do
              S.marker ! A.id_ "arrow"
                       ! A.viewbox "0 0 10 10"
                       ! A.refx "1"
                       ! A.refy "5"
                       ! A.markerunits "strokeWidth"
                       ! A.orient "auto"
                       ! A.markerwidth "7"
                       ! A.markerheight "7" $ do
                S.polyline ! A.points "0,1 10,5 0,9" ! A.fill "black"

-- | Builds an SVG document.
buildSVG :: IO ()
buildSVG = 
    runSqlite dbStr $ do
        sqlRects    :: [Entity Rects]    <- selectList [] []
        sqlTexts    :: [Entity Texts]    <- selectList [] []
        sqlPaths    :: [Entity Paths]    <- selectList [] []
        sqlEllipses :: [Entity Ellipses] <- selectList [] []

        let texts       = map (buildText . entityVal) sqlTexts
        let rects       = map (buildRect texts . entityVal) sqlRects
        let ellipses    = buildEllipses texts 0 $ map entityVal sqlEllipses
        let paths       = zipWith (buildPath rects ellipses) (map entityVal sqlPaths) [1..length sqlPaths]
        let regions     = filter pathIsRegion paths
        let edges       = filter (not . pathIsRegion) paths
        let regionTexts = filter (not . intersectsWithShape (rects ++ ellipses)) texts

        let stringSVG = renderSvg $ makeSVGDoc rects ellipses edges regions regionTexts
        liftIO $ writeFile "Testfile.svg.2" stringSVG

intersectsWithShape :: [Shape] -> Text -> Bool
intersectsWithShape shapes text = do
    length (filter (intersectsWithPoint (fromRational $ textXPos text) (fromRational $ textYPos text)) shapes) > 0

-- | Converts a `Rect` to SVG.
convertRectToSVG :: Shape -> S.Svg
convertRectToSVG rect = if shapeFill rect == "none" then S.rect else
                        S.g ! A.id_ (stringValue $ shapeId rect)
                            ! A.class_ (stringValue $ if shapeIsHybrid rect then "hybrid" else "node")
                            ! S.customAttribute "data-group" (stringValue (getArea (shapeId rect))) $
                            do S.rect ! A.rx "4"
                                      ! A.ry "4"
                                      ! A.x (stringValue $ show $ fromRational $ shapeXPos rect)
                                      ! A.y (stringValue $ show $ fromRational $ shapeYPos rect)
                                      ! A.width (stringValue $ show $ fromRational $ shapeWidth rect)
                                      ! A.height (stringValue $ show $ fromRational $ shapeHeight rect)
                                      ! A.style (stringValue $ "fill:" ++ (getFill (shapeId rect)) ++ ";")
                               concatSVG $ map (convertTextToSVG (shapeIsHybrid rect) False False) (shapeText rect)

-- | Converts a `Text` to SVG.
convertTextToSVG :: Bool -> Bool -> Bool -> Text -> S.Svg
convertTextToSVG isHybrid isBool isRegion text =
    S.text_ ! A.x (stringValue $ show $ fromRational $ textXPos text)
            ! A.y (stringValue $ show $ fromRational $ textYPos text)
            ! A.style (stringValue $
                      (if isHybrid then hybridTextStyle else (if isBool then ellipseTextStyle else (if isRegion then regionTextStyle else ""))) ++ "font-family:sans-serif;")
            $ toMarkup $ textText text

-- | Converts a `Path` to SVG.
convertEdgeToSVG :: Path -> S.Svg
convertEdgeToSVG path =
    S.path ! A.id_ (stringValue $ "path" ++ (pathId path))
           ! A.class_ (stringValue $ "path")
           ! A.d (stringValue $ "M" ++ (buildPathString $ points path))
           ! A.markerEnd (stringValue $ "url(#arrow)")
           ! S.customAttribute "source-node" (stringValue $ source path)
           ! S.customAttribute "target-node" (stringValue $ target path)
           ! A.style (stringValue $ "fill:" ++
                      pathFill path ++
                      ";fill-opacity:" ++
                      pathFillOpacity path ++
                      ";")

-- | Converts a `Path` to SVG.
convertRegionToSVG :: Path -> S.Svg
convertRegionToSVG path =
    S.path ! A.id_ (stringValue $ "region" ++ (pathId path))
           ! A.class_ (stringValue $ "region")
           ! A.d (stringValue $ "M" ++ (buildPathString $ points path))
           ! A.style (stringValue $ "fill:" ++
                      pathFill path ++
                      ";opacity:0.7;fill-opacity:0.58;")

-- | Converts an `Ellipse` to SVG.
convertEllipseToSVG :: Shape -> S.Svg
convertEllipseToSVG ellipse = S.g ! A.id_ (stringValue (shapeId ellipse))
                                  ! A.class_ "bool" $ do
                                      S.ellipse ! A.cx (stringValue $ show $ fromRational $ shapeXPos ellipse)
                                                ! A.cy (stringValue $ show $ fromRational $ shapeYPos ellipse)
                                                ! A.rx (stringValue $ show $ (fromRational $ shapeWidth ellipse) / 2)
                                                ! A.ry (stringValue $ show $ (fromRational $ shapeHeight ellipse) / 2)
                                                ! A.style "stroke:#000000;fill:none;"
                                      concatSVG $ map (convertTextToSVG False True False) (shapeText ellipse)