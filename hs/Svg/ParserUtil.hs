-- |Provide some utility functions for the SVG parsing.
module Svg.ParserUtil where

import Database.Tables (Point)

-- | Adds two tuples together.
addTuples :: Point -> Point -> Point
addTuples (a,b) (c,d) = (a + c, b + d)
