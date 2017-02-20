{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}

{-|
Description: Helpers for enabling graph interactitivy.

This module takes the raw data parsed from the SVG files and computes
prerequisite relationships based on the geometry.

This is currently done after the data is first inserted, when the new
SVG is generated. This work should really be done immediately after
parsing, before anything is inserted into the database.
-}

module Svg.Builder
    (buildPath,
     buildRect,
     buildEllipses,
     intersectsWithShape,
     buildPathString,
     sanitizeId) where

import Data.Char (toLower)
import Data.List (find)
import Database.Tables hiding (texts, shapes)
import Database.DataType

-- * Builder functions

-- | Fills in the id and source and target nodes of the path.
-- It is debateable whether the id is necessary, but the source
-- and targets must be set after the rectangles have been parsed.
buildPath :: [Shape] -- ^ Node elements.
          -> [Shape] -- ^ Ellipses.
          -> Path    -- ^ A path.
          -> Integer -- ^ A number to use in the ID of the path.
          -> Path
buildPath rects ellipses entity elementId
    | pathIsRegion entity =
          entity {pathId_ = pathId_ entity ++ ('p' : show elementId),
                  pathSource = "",
                  pathTarget = ""}
    | otherwise =
          let coords = pathPoints entity
              start = head coords
              end = last coords
              sourceNode = getIntersectingShape start (rects ++ ellipses)
              targetNode = getIntersectingShape end
                               (filter (\r -> shapeId_ r /= sourceNode) rects ++
                                ellipses)
          in
              entity {pathId_ = 'p' : show elementId,
                      pathSource = sourceNode,
                      pathTarget = targetNode}

-- | Builds a Rect from a database entry.
-- Fills in the text association(s) and ID.
buildRect :: [Text]  -- ^ A list of shapes that may intersect with the given node.
          -> Shape   -- ^ A node.
          -> Integer -- ^ An integer to uniquely identify the shape
          -> Shape
buildRect texts entity elementId =
    let rectTexts = filter (intersects
                            (shapeWidth entity)
                            (shapeHeight entity)
                            (shapePos entity)
                            0  -- no tolerance for text intersection
                            . textPos
                            ) texts
        textString = concatMap textText rectTexts
        id_ = case shapeType_ entity of
              Hybrid -> 'h' : show elementId
              Node -> map toLower $ sanitizeId textString
              other
    in
        entity {shapeId_ = id_,
                shapeText = rectTexts,
                -- TODO: check if already set this one during parsing
                shapeTolerance = 9}

-- | Builds an ellipse from a database entry.
-- Fills in the text association and ID.
buildEllipses :: [Text]  -- ^ A list of Text elements that may or may not intersect
                         --   with the given ellipse.
              -> Shape   -- ^ An ellipse.
              -> Integer -- ^ A number to use in the ID of the ellipse.
              -> Shape
buildEllipses texts entity elementId =
    let ellipseText = filter (intersectsEllipse
                              (shapeWidth entity / 2)
                              (shapeHeight entity / 2)
                              (fst (shapePos entity) - shapeWidth entity / 2,
                               snd (shapePos entity) - shapeHeight entity / 2)
                              . textPos
                              ) texts
    in
        entity {shapeId_ = "bool" ++ show elementId,
                shapeFill = "", -- TODO: necessary?
                shapeText = ellipseText,
                shapeTolerance = 20} -- TODO: necessary?
    where
        intersectsEllipse a b (cx, cy) (x, y) =
            let dx = x - cx - 5  -- some tolerance
                dy = y - cy - 5
            in
                (dx*dx) / (a*a) + (dy*dy) / (b*b) < 1

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [Point] -> String
buildPathString d = unwords $ map toString d
    where
        toString (a, b) = show a ++ "," ++ show b


-- * Intersection helpers

-- | Determines if a point is contained in a given rectangular region.
intersects :: Double -- ^ The region's width.
           -> Double -- ^ The region's height.
           -> Point  -- ^ The region's bottom-left coordinate.
           -> Double -- ^ The tolerance for the point being outside the boundary.
           -> Point  -- ^ The point's coordinate.
           -> Bool
intersects width height (rx, ry) offset (px, py) =
    let dx = px - rx
        dy = py - ry
    in  dx >= -1 * offset &&
        dx <= width + offset &&
        dy >= -1 * offset &&
        dy <= height + offset;

-- | Determines if a point is contained in a shape.
intersectsWithPoint :: Point -> Shape -> Bool
intersectsWithPoint point shape
    | shapeType_ shape == BoolNode =
        intersects (shapeWidth shape)
                   (shapeHeight shape)
                   (fst (shapePos shape) - shapeWidth shape / 2,
                    snd (shapePos shape) - shapeHeight shape / 2)
                   (shapeTolerance shape)
                   point
    | otherwise =
        intersects (shapeWidth shape)
                     (shapeHeight shape)
                     (shapePos shape)
                     (shapeTolerance shape)
                     point

-- | Returns the ID of the first shape in a list that intersects
-- with the given point.
getIntersectingShape :: Point -> [Shape] -> String
getIntersectingShape point shapes =
    maybe "" shapeId_ $ find (intersectsWithPoint point) shapes

-- | Determines if a text intersects with any shape in a list.
intersectsWithShape :: [Shape] -> Text -> Bool
intersectsWithShape shapes text =
    any (intersectsWithPoint (textPos text)) shapes

-- ** Other helpers

-- | Strips disallowed characters from string for DOM id
sanitizeId :: String -> String
sanitizeId = filter (\c -> not $ elem c (",()/<>% " :: String))
