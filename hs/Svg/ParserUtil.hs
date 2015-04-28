-- |Provide some utility functions for the SVG parsing.
module Svg.ParserUtil where

import Database.Tables (Point)

-- | Adds two tuples together.
addTuples :: Point -> Point -> Point
addTuples (a,b) (c,d) = (a + c, b + d)

-- | Determines if a point intersects with a shape.
intersects :: Double -- ^ The shape's width.
           -> Double -- ^ The shape's height.
           -> Point  -- ^ The shape's coordinate.
           -> Double -- ^ The offset.
           -> Point  -- ^ The point's coordinate.
           -> Bool
intersects width height (rx, ry) offset (px, py) =
    let dx = px - rx
        dy = py - ry
    in dx >= -1 * offset &&
       dx <= width + offset &&
       dy >= -1 * offset &&
       dy <= height + offset;
