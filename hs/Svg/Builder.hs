{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}

{-|
Description: Helpers for enabling graph interactitivy.

This module takes the raw data parsed from the SVG files and computes
prerequisite relationships based on the geometry.

This is currently done after the data is first inserted, when the new
SVG is generated. This work should really be done immediately after
parsing, before anything is inserted into the database.
-}

module Svg.Builder where

import Data.Char (toLower)
import Data.List (find)
import Database.Tables
import Database.DataType
import Data.Int (Int64)

-- * Builder functions

-- | Fills in the id and source and target nodes of the path.
-- It is debateable whether the id is necessary, but the source
-- and targets must be set after the rectangles have been parsed.
buildPath :: [Shape] -- ^ Node elements.
          -> [Shape] -- ^ Ellipses.
          -> Path    -- ^ A path.
          -> Int64   -- ^ A number to use in the ID of the path.
          -> Path
buildPath rects ellipses entity idCounter
    | pathIsRegion entity =
          entity {pathId_ = pathId_ entity ++ ('p' : show idCounter),
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
              entity {pathId_ = 'p' : show idCounter,
                      pathSource = sourceNode,
                      pathTarget = targetNode}

-- | Builds a Rect from a database entry.
-- Fills in the text association(s) and ID.
buildRect :: [Text]  -- ^ A list of shapes that may intersect with the given node.
          -> Shape   -- ^ A node.
          -> Int64   -- ^ An integer to uniquely identify the shape
          -> Shape
buildRect texts entity idCounter =
    let rectTexts = filter (intersects
                            (shapeWidth entity)
                            (shapeHeight entity)
                            (shapePos entity)
                            9
                            . textPos
                            ) texts
        textString = concatMap textText rectTexts
        -- TODO: consolidate with toId in Generator.hs
        sanitize = filter (\c -> not $ elem c ",()/<>%")
        id_ = case shapeType_ entity of
              Hybrid -> "h" ++ show idCounter
              Node -> map toLower $ sanitize textString
              BoolNode -> error "Pattern matching buildRect BoolNode"
              Region -> error "Pattern matching buildRect Region"
    in
        entity {shapeId_ = id_,
                shapeText = rectTexts,
                -- TODO: check if already set this one during parsing
                shapeTolerance = 9}

-- | Builds an ellipse from a database entry.
-- Fills in the text association and ID.
buildEllipses :: [Text] -- ^ A list of Text elements that may or may not intersect
                        --   with the given ellipse.
              -> Shape  -- ^ An ellipse.
              -> Int64  -- ^ A number to use in the ID of the ellipse.
              -> Shape
buildEllipses texts entity idCounter =
    let ellipseText = filter (intersects
                              (shapeWidth entity)
                              (shapeHeight entity)
                              (shapePos entity)
                              9 -- TODO: Should this be 9 or 20?
                              . textPos
                              ) texts
    in
        entity {shapeId_ = "bool" ++ show idCounter,
                shapeFill = "", -- TODO: necessary?
                shapeText = ellipseText,
                shapeTolerance = 20} -- TODO: necessary?

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
intersectsWithPoint point shape =
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
