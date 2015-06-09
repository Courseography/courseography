{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables #-}

{-|
Description: Generate a new SVG file from the database graph representation.

This module is responsible for taking the database representation of a graph
and creating a new SVG file.

This functionality is used both to create SVG for the Graph component,
as well as generating images on the fly for Facebook posting.
-}

module Svg.Generator where

import Svg.Builder
import Database.Tables
import Database.DataType
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite
import Data.List hiding (map, filter)
import MakeElements
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Internal (stringValue)
import Text.Blaze (toMarkup)
import Css.Constants (theoryDark,
                      seDark,
                      systemsDark,
                      hciDark,
                      graphicsDark,
                      numDark,
                      aiDark,
                      introDark,
                      mathDark)
import qualified Data.Map.Strict as M
import Data.Monoid (mempty)
import Config (dbStr)

-- | This is the main function that retrieves a stored graph
-- from the database and creates a new SVG file for it.
buildSVG :: GraphId              -- ^ The ID of the graph that is being built.
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
                                              ShapeGraph ==. gId] []
        sqlTexts    :: [Entity Text]  <- selectList [TextGraph ==. gId] []
        sqlPaths    :: [Entity Path]  <- selectList [PathGraph ==. gId] []
        sqlEllipses :: [Entity Shape] <- selectList
                                             [ShapeType_ ==. BoolNode,
                                              ShapeGraph ==. gId] []
        let courseStyleMap = M.map convertSelectionToStyle courseMap
            texts          = map entityVal sqlTexts
            -- TODO: Ideally, we would do these "build" steps *before*
            -- inserting these values into the database.
            rects          = zipWith (buildRect texts)
                                     (map entityVal sqlRects)
                                     (map keyAsInt sqlRects)
            ellipses       = zipWith (buildEllipses texts)
                                     (map entityVal sqlEllipses)
                                     (map keyAsInt sqlEllipses)
            paths          = zipWith (buildPath rects ellipses)
                                     (map entityVal sqlPaths)
                                     (map keyAsInt sqlPaths)
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
    where
        keyAsInt :: PersistEntity a => Entity a -> Integer
        keyAsInt = fromIntegral . (\(PersistInt64 x) -> x) . head . keyToValues . entityKey

        convertSelectionToStyle :: String -> String
        convertSelectionToStyle courseStatus =
            if isSelected courseStatus
            then "stroke-width:4;"
            else "opacity:0.5;stroke-dasharray:8,5;"

        isSelected courseStatus =
            isPrefixOf "active" courseStatus ||
            isPrefixOf "overridden" courseStatus

-- * SVG Creation

-- | This function does the heavy lifting to actually create
-- a new SVG value given the graph components.
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
                          concatSVG $ map (regionToSVG styled)
                                          regions
                      S.g ! A.id_ "nodes"
                          ! A.style "stroke:#000000;" $
                          concatSVG $ map (rectToSVG styled courseMap)
                                           rects
                      S.g ! A.id_ "bools" $
                          concatSVG $ map (ellipseToSVG styled)
                                          ellipses
                      S.g ! A.id_ "edges"
                          ! A.style "stroke:#000000" $
                              concatSVG $ map (edgeToSVG styled)
                                              edges
                      S.g ! A.id_ "region-labels" $
                          concatSVG $ map (textToSVG styled Region 0)
                                          regionTexts

-- | Builds the SVG defs. Currently, we only use a single one, for the arrowheads.
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

-- | Converts a node to SVG.
rectToSVG :: Bool -> M.Map String String -> Shape -> S.Svg
rectToSVG styled courseMap rect
    | shapeFill rect == "none" = S.rect
    | otherwise =
        let style = case shapeType_ rect of
                        Node -> fromMaybe "" $ M.lookup (shapeId_ rect)
                                                        courseMap
                        _    -> ""
            class_ = case shapeType_ rect of
                         Node -> "node"
                         Hybrid -> "hybrid"
        in S.g ! A.id_ (stringValue $ toId $ shapeId_ rect)
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
                         ! A.style (stringValue $ "fill:" ++ shapeFill rect ++ ";")
                  concatSVG $ map
                      (textToSVG
                          styled
                          (shapeType_ rect)
                          (fst (shapePos rect) + (shapeWidth rect / 2)))
                      (shapeText rect)

-- | Converts an ellipse to SVG.
ellipseToSVG :: Bool -> Shape -> S.Svg
ellipseToSVG styled ellipse =
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
            concatSVG $ map
                (textToSVG styled BoolNode (fst $ shapePos ellipse))
                (shapeText ellipse)

-- | Converts a text value to SVG.
textToSVG :: Bool -> ShapeType -> Double -> Text -> S.Svg
textToSVG styled type_ xPos' text =
    S.text_ ! A.x (stringValue $ show $
                      if type_ == Region
                      then xPos
                      else xPos')
            ! A.y (stringValue $ show yPos)
            ! A.style (stringValue $ align ++ fontStyle ++ fill)
            $ toMarkup $ textText text
    where
        (xPos, yPos) = textPos text
        alignVal = case type_ of
                       Region -> textAlign text
                       _ -> "middle"
        align = "text-anchor:" ++ alignVal ++ ";"
        fill = case textFill text of
               "" -> ""
               colour -> "fill:" ++ colour ++ ";"

        getTextStyle Hybrid    = hybridTextStyle
        getTextStyle BoolNode  = ellipseTextStyle
        getTextStyle Region    = regionTextStyle
        getTextStyle _         = ""

        -- TODO: Possibly move this closer to the CSS
        hybridTextStyle = "font-size:7.5pt;fill:white;"
        ellipseTextStyle = "font-size:7.5pt;"
        regionTextStyle = "font-size:9pt;"

        fontStyle = if styled
                    then
                        getTextStyle type_ ++
                        "font-family:sans-serif;stroke:none;"
                    else
                        ""

-- | Converts a path to SVG.
edgeToSVG :: Bool -> Path -> S.Svg
edgeToSVG styled path =
    S.path ! A.id_ (stringValue $ "path" ++ pathId_ path)
           ! A.class_ "path"
           ! A.d (stringValue $ 'M' : buildPathString (pathPoints path))
           ! A.markerEnd "url(#arrow)"
           ! S.customAttribute "source-node" (stringValue $ toId
                                                          $ pathSource path)
           ! S.customAttribute "target-node" (stringValue $ toId
                                                          $ pathTarget path)
           ! if styled
             then
                 A.style (stringValue $ "fill:" ++
                          pathFill path ++
                          ";fill-opacity:1;")
             else
                 mempty

-- | Converts a region to SVG.
regionToSVG :: Bool -> Path -> S.Svg
regionToSVG styled path =
    S.path ! A.id_ (stringValue $ "region" ++ pathId_ path)
           ! A.class_ "region"
           ! A.d (stringValue $ 'M' : buildPathString (pathPoints path))
           ! A.style (stringValue $ "fill:" ++ pathFill path ++ ";" ++
                      if styled
                      then
                          ";opacity:0.7;fill-opacity:0.58;"
                      else "")


-- ** Hard-coded map definitions (should be removed, eventually)

-- | Gets a tuple from areaMap where id_ is in the list of courses for that
-- tuple.
getTuple :: String -- ^ The course's ID.
         -> Maybe (T.Text, String)
getTuple id_
    | M.null tuples = Nothing
    | otherwise   = Just $ snd $ M.elemAt 0 tuples
    where tuples = M.filterWithKey (\k _ -> id_ `elem` k) areaMap

-- | Gets an area from areaMap where id_ is in the list of courses for the
-- corresponding tuple.
getArea :: String -> String
getArea id_ = maybe "" snd $ getTuple id_

-- | A list of tuples that contain disciplines (areas), fill values, and courses
-- that are in the areas.
-- TODO: Remove colour dependencies, and probably the whole map.
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

-- ** Other helpers

-- | Strip disallowed characters from string for DOM id
toId :: String -> String
toId = filter (\c -> not $ elem c ",()/<>%")
