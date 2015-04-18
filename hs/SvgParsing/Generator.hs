{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables #-}
module SvgParsing.Generator where

import SvgParsing.Builder
import SvgParsing.ParserUtil
import Database.Tables
import Database.DataType
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite
import Data.List hiding (map, filter)
import Data.Int
import Database.JsonParser
import MakeElements
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Internal (stringValue)
import Text.Blaze (toMarkup)
import Css.Constants
import qualified Data.Map.Strict as M
import Data.Monoid (mempty)

-- | Builds an SVG document.
makeSVGDoc :: M.Map String String
           -> [Shape] -- ^ A list of the Nodes that will be included
                      --   in the graph. This includes both Hybrids and
                      --   course nodes.
           -> [Shape] -- ^ A list of the Ellipses that will be included
                      --   in the graph.
           -> [Path]  -- ^ A list of the Edges that will be included
                      --   in the graph.
           -> [Path]  -- ^ A list of the Regions that will be included
                      --   in the graph.
           -> [Text]  -- ^ A list of the 'Text' elements that will be included
                      --   in the graph.
           -> Bool    -- ^ Whether to include inline styles in the graph.
           -> S.Svg   -- ^ The completed SVG document.
makeSVGDoc courseMap rects ellipses edges regions regionTexts styled =
    S.docTypeSvg ! A.width "1195"
                 ! A.height "650"
                 ! S.customAttribute "xmlns:svg" "http://www.w3.org/2000/svg"
                 ! S.customAttribute "xmlns:dc" "http://purl.org/dc/elements/1.1/"
                 ! S.customAttribute "xmlns:cc" "http://creativecommons.org/ns#"
                 ! S.customAttribute "xmlns:rdf"
                                     "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 ! A.version "1.1" $ do
                      makeSVGDefs
                      S.g ! A.id_ "regions" $
                          concatSVG $ map (convertRegionToSVG styled)
                                          regions
                      S.g ! A.id_ "nodes"
                          ! A.style "stroke:#000000;" $
                              concatSVG $ map (convertRectToSVG styled courseMap)
                                              rects
                      S.g ! A.id_ "bools" $
                          concatSVG $ map (convertEllipseToSVG styled)
                                          ellipses
                      S.g ! A.id_ "edges"
                          ! A.style "stroke:#000000" $
                              concatSVG $ map (convertEdgeToSVG styled)
                                              edges
                      S.g ! A.id_ "region-labels" $
                          concatSVG $ map (convertTextToSVG styled Region)
                                          regionTexts

-- | Builds an SVG document.
buildSVG :: Int64                -- ^ The ID of the graph that is being built.
         -> M.Map String String  -- ^ A map of courses that holds the course
                                 --   ID as a key, and the data-active
                                 --   attribute as the course's value.
                                 --   The data-active attribute is used in the
                                 --   interactive graph to indicate which
                                 --   courses the user has selected.
         -> String               -- ^ The filename that this graph will be
                                 --   written to.
         -> Bool                 -- ^ Whether to include inline styles.
         -> IO ()
buildSVG gId courseMap filename styled =
    runSqlite dbStr $ do
        sqlRects    :: [Entity Shape] <- selectList
                                             [ShapeType_ <-. [Node, Hybrid],
                                              ShapeGId ==. gId] []
        sqlTexts    :: [Entity Text]  <- selectList [TextGId ==. gId] []
        sqlPaths    :: [Entity Path]  <- selectList [PathGId ==. gId] []
        sqlEllipses :: [Entity Shape] <- selectList
                                             [ShapeType_ ==. BoolNode,
                                              ShapeGId ==. gId] []
        let courseStyleMap = M.map convertSelectionToStyle courseMap
            texts          = map entityVal sqlTexts
            rects          = map (buildRect texts . entityVal) sqlRects
            ellipses       = zipWith (buildEllipses texts)
                                     (map entityVal sqlEllipses) [1..]
            paths          = zipWith (buildPath rects ellipses)
                                     (map entityVal sqlPaths) [1..]
            regions        = filter pathIsRegion paths
            edges          = filter (not . pathIsRegion) paths
            regionTexts    = filter (not .
                                     intersectsWithShape (rects ++ ellipses))
                                    texts
            stringSVG      = renderSvg $ makeSVGDoc courseStyleMap
                                                    rects
                                                    ellipses
                                                    edges
                                                    regions
                                                    regionTexts
                                                    styled
        liftIO $ writeFile filename stringSVG

-- | Gets a tuple from areaMap where id_ is in the list of courses for that
--   tuple.
getTuple :: String -- ^ The course's ID.
         -> Maybe (T.Text, String)
getTuple id_
    | M.null tuples = Nothing
    | otherwise   = Just $ snd $ M.elemAt 0 tuples
    where tuples = M.filterWithKey (\k _ -> id_ `elem` k) areaMap

-- | Gets an area from areaMap where id_ is in the list of courses for the
--   corresponding tuple.
getArea :: String -> String
getArea id_ = maybe "" snd $ getTuple id_

-- | Gets the fill from areaMap where id_ is in the list of courses for the
--   corresponding tuple.
getFill :: String -- ^ The course's ID.
        -> String -- ^ The course's allocated fill value.
getFill id_ = maybe "grey" (T.unpack . fst) $ getTuple id_

-- | Builds the SVG defs.
makeSVGDefs :: S.Svg
makeSVGDefs =
    S.defs $
    S.marker ! A.id_ "arrow"
             ! A.viewbox "0 0 10 10"
             ! A.refx "4"
             ! A.refy "5"
             ! A.markerunits "strokeWidth"
             ! A.orient "auto"
             ! A.markerwidth "7"
             ! A.markerheight "7" $
                 S.polyline ! A.points "0,1 10,5 0,9"
                            ! A.fill "black"

-- | Determines if a text intersects with a shape.
intersectsWithShape :: [Shape] -> Text -> Bool
intersectsWithShape shapes text =
    any (intersectsWithPoint (textPos text)) shapes

-- | Converts a `Rect` to SVG.
convertRectToSVG :: Bool -> M.Map String String -> Shape -> S.Svg
convertRectToSVG styled courseMap rect
    | shapeFill rect == "none" = S.rect
    | otherwise =
        let style = case shapeType_ rect of
                        Node -> fromMaybe "" $ M.lookup (shapeId_ rect)
                                                        courseMap
                        _    -> ""
            class_ = case shapeType_ rect of
                         Node -> "node"
                         Hybrid -> "hybrid"
        in S.g ! A.id_ (stringValue $ filter (/=',') $ shapeId_ rect)
               ! A.class_ (stringValue class_)
               ! S.customAttribute "data-group" (stringValue
                                                 (getArea (shapeId_ rect)))
               ! S.customAttribute "text-rendering" "geometricPrecision"
               ! S.customAttribute "shape-rendering" "geometricPrecision"
               -- TODO: Remove the reliance on the colours here
               ! (if styled || class_ /= "hybrid"
                  then
                      A.style (stringValue style)
                  else
                      mempty)
               $
               do S.rect ! A.rx "4"
                         ! A.ry "4"
                         ! A.x (stringValue . show . fst $ shapePos rect)
                         ! A.y (stringValue . show . snd $ shapePos rect)
                         ! A.width (stringValue . show $ shapeWidth rect)
                         ! A.height (stringValue . show $ shapeHeight rect)
                         ! if styled || class_ /= "hybrid"
                           then A.style (stringValue $ "fill:" ++
                                         getFill (shapeId_ rect) ++
                                         ";")
                           else
                               mempty
                  concatSVG $ map (convertTextToSVG styled (shapeType_ rect))
                                  (shapeText rect)

-- | Maps styles to courses based on the courses status.
convertSelectionToStyle :: String -> String
convertSelectionToStyle courseStatus =
    if isSelected courseStatus
    then "stroke-width:4;"
    else "opacity:0.5;stroke-dasharray:8,5;"

-- | Determines whether a course is 'selected'.
isSelected :: String -- ^ The selected status of a course.
           -> Bool
isSelected courseStatus =
    isPrefixOf "active" courseStatus ||
    isPrefixOf "overridden" courseStatus

-- | Converts a `Text` to SVG.
convertTextToSVG :: Bool -> ShapeType -> Text -> S.Svg
convertTextToSVG styled type_ text =
    S.text_ ! A.x (stringValue $ show xPos)
            ! A.y (stringValue $ show yPos)
            ! (if styled
              then
                  A.style (stringValue $
                           getTextStyle type_ ++
                           "font-family:sans-serif;stroke:none;")
              else
                  mempty)
            $ toMarkup $ textText text
    where (xPos, yPos) = textPos text

-- | Converts a `Path` to SVG.
convertEdgeToSVG :: Bool -> Path -> S.Svg
convertEdgeToSVG styled path =
    S.path ! A.id_ (stringValue $ "path" ++ pathId_ path)
           ! A.class_ "path"
           ! A.d (stringValue $ 'M' : buildPathString (pathPoints path))
           ! A.markerEnd "url(#arrow)"
           ! S.customAttribute "source-node" (stringValue $ filter (/=',')
                                                          $ pathSource path)
           ! S.customAttribute "target-node" (stringValue $ filter (/=',')
                                                          $ pathTarget path)
           ! if styled
             then
                 A.style (stringValue $ "fill:" ++
                          pathFill path ++
                          ";fill-opacity:1;")
             else
                 mempty

-- | Converts a `Path` to SVG.
convertRegionToSVG :: Bool -> Path -> S.Svg
convertRegionToSVG styled path =
    S.path ! A.id_ (stringValue $ "region" ++ pathId_ path)
           ! A.class_ "region"
           ! A.d (stringValue $ 'M' : buildPathString (pathPoints path))
           ! A.style (stringValue $ "fill:" ++ pathFill path ++ ";" ++
                      if styled
                      then
                          ";opacity:0.7;fill-opacity:0.58;"
                      else "")

-- | Converts an `Ellipse` to SVG.
convertEllipseToSVG :: Bool -> Shape -> S.Svg
convertEllipseToSVG styled ellipse =
    S.g ! A.id_ (stringValue (shapeId_ ellipse))
        ! A.class_ "bool" $ do
            S.ellipse ! A.cx (stringValue . show . fst $ shapePos ellipse)
                      ! A.cy (stringValue . show . snd $ shapePos ellipse)
                      ! A.rx (stringValue . show $ shapeWidth ellipse / 2)
                      ! A.ry (stringValue . show $ shapeHeight ellipse / 2)
                      ! if styled
                        then
                            A.style "stroke:#000000;fill:none;"
                        else mempty
            concatSVG $ map (convertTextToSVG styled BoolNode) (shapeText ellipse)

getTextStyle :: ShapeType -- ^ The parent element of the Text element in
                          --   question.
             -> String
getTextStyle Hybrid    = hybridTextStyle
getTextStyle BoolNode  = ellipseTextStyle
getTextStyle Region    = regionTextStyle
getTextStyle _         = ""

-- | A list of tuples that contain disciplines (areas), fill values, and courses
---  that are in the areas.
areaMap :: M.Map [String] (T.Text, String)
areaMap =  M.fromList
           [
           (["csc165", "csc236", "csc240", "csc263", "csc265",
             "csc310", "csc373", "csc438", "csc448",
             "csc463"], (theoryDark, "theory")),
           (["csc207", "csc301", "csc302", "csc410", "csc465",
             "csc324"], (seDark, "se")),
           (["csc209", "csc258", "csc358", "csc369", "csc372",
             "csc458", "csc469", "csc488", "ece385", "ece489",
             "csc309", "csc343", "csc443"],
             (systemsDark, "systems")),
           (["csc200", "csc300",  "csc318", "csc404", "csc428",
             "csc454"], (hciDark, "hci")),
           (["csc320", "csc418", "csc420"], (graphicsDark, "graphics")),
           (["csc336", "csc436", "csc446", "csc456", "csc466"],
            (numDark, "num")),
           (["csc321", "csc384", "csc401", "csc411", "csc412",
             "csc485", "csc486"], (aiDark, "ai")),
           (["csc104", "csc120", "csc108", "csc148"], (introDark, "intro")),
           (["calc1", "lin1", "sta1", "sta2"], (mathDark, "math"))]

-- | The style for Text elements of hybrids.
hybridTextStyle :: String
hybridTextStyle = "font-size:7.5pt;fill:white;"

-- | The style for Text elements of ellipses.
ellipseTextStyle :: String
ellipseTextStyle = "font-size:7.5pt;"

-- | The style for Text elements of regions.
regionTextStyle :: String
regionTextStyle = "font-size:9pt;"
