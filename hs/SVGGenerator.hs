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
import qualified Data.Text as T
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import SVGBuilder
import Text.Blaze.Internal (stringValue)
import Text.Blaze (toMarkup)

makeSVGDoc :: [Shape] -> [Shape] -> [Path] -> [Path] -> S.Svg
makeSVGDoc rects ellipses edges regions =
    S.docTypeSvg ! A.width "1052.3622"
                 ! A.height "744.09448"
                 ! A.version "1.1" $ do
                      makeSVGDefs
                      S.g ! A.id_ "nodes" $ do
                          concatSVG $ map convertRegionToSVG regions
                          concatSVG $ map convertRectToSVG rects
                          concatSVG $ map convertEllipseToSVG ellipses
                          S.g ! A.style "stroke:#000000" $ concatSVG $ map convertEdgeToSVG edges

makeSVGDefs :: S.Svg
makeSVGDefs = S.defs $ do
              S.marker ! A.id_ "arrow"
                       ! A.viewbox "0 0 10 10"
                       ! A.refx "1"
                       ! A.refy "5"
                       ! A.markerunits "strokeWidth"
                       ! A.orient "auto"
                       ! A.markerwidth "4.5"
                       ! A.markerheight "4.5" $ do
                S.polyline ! A.points "0,1 10,5 0,9" ! A.fill "black"

-- | Builds an SVG document.
buildSVG :: IO ()
buildSVG = 
    runSqlite dbStr $ do
        sqlRects    :: [Entity Rects]    <- selectList [] []
        sqlTexts    :: [Entity Texts]    <- selectList [] []
        sqlPaths    :: [Entity Paths]    <- selectList [] []
        sqlEllipses :: [Entity Ellipses] <- selectList [] []

        let texts      = map (buildText . entityVal) sqlTexts
        let paths      = buildPaths 0 $ map entityVal sqlPaths
        let regions    = filter pathIsRegion paths
        let edges      = filter (not . pathIsRegion) paths
        let rects      = map (buildRect texts . entityVal) sqlRects
        let ellipses   = buildEllipses texts 0 $ map entityVal sqlEllipses

        let processedEdges = map (processPath rects ellipses) edges

        let stringSVG = renderSvg $ makeSVGDoc rects ellipses processedEdges regions
        liftIO $ writeFile "Testfile.svg.2" stringSVG

-- | Converts a `Rect` to SVG.
convertRectToSVG :: Shape -> S.Svg
convertRectToSVG rect = if shapeFill rect == "none" then S.rect else
                        S.g ! A.id_ (stringValue $ shapeId rect)
                            ! A.class_ (stringValue $ if shapeIsHybrid rect then "hybrid" else "node") $
                            do S.rect ! A.rx "4"
                                      ! A.ry "4"
                                      ! A.x (stringValue $ show $ fromRational $ shapeXPos rect)
                                      ! A.y (stringValue $ show $ fromRational $ shapeYPos rect)
                                      ! A.width (stringValue $ show $ fromRational $ shapeWidth rect)
                                      ! A.height (stringValue $ show $ fromRational $ shapeHeight rect)
                                      ! A.style (stringValue $ "fill:" ++
                                                 shapeFill rect ++
                                                 ";stroke:#000000;")
                               concatSVG $ map convertTextToSVG (shapeText rect)

-- | Converts a `Text` to SVG.
convertTextToSVG :: Text -> S.Svg
convertTextToSVG text =
    S.text_ ! A.x (stringValue $ show $ fromRational $ textXPos text)
            ! A.y (stringValue $ show $ fromRational $ textYPos text)
            ! A.style (stringValue $ "font-size:" ++
                      textFontSize text ++
                      ";font-weight:" ++
                      textFontWeight text ++
                      ";font-family:" ++
                      textFontFamily text ++
                      ";")
            $ toMarkup $ textText text

-- | Converts a `Path` to XML.
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

-- | Converts a `Path` to XML.
convertRegionToSVG :: Path -> S.Svg
convertRegionToSVG path =
    S.path ! A.id_ (stringValue $ "region" ++ (pathId path))
           ! A.class_ (stringValue $ "region")
           ! A.d (stringValue $ "M" ++ (buildPathString $ points path))
           ! A.style (stringValue $ "fill:" ++
                      pathFill path ++
                      ";fill-opacity:" ++
                      pathFillOpacity path ++
                      ";")


-- | Converts an `Ellipse` to XML.
convertEllipseToSVG :: Shape -> S.Svg
convertEllipseToSVG ellipse = S.g ! A.id_ (stringValue (shapeId ellipse))
                                  ! A.class_ "bool" $ do
                                      S.ellipse ! A.cx (stringValue $ show $ fromRational $ shapeXPos ellipse)
                                                ! A.cy (stringValue $ show $ fromRational $ shapeYPos ellipse)
                                                ! A.rx (stringValue $ show $ (fromRational $ shapeWidth ellipse) / 2)
                                                ! A.ry (stringValue $ show $ (fromRational $ shapeHeight ellipse) / 2)
                                                ! A.style "stroke:#000000;fill:none;"
                                      concatSVG $ map convertTextToSVG (shapeText ellipse)