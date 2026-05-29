{-|
    Module      : Util.Matrix
    Description : Contains a single method that applies a matrix transformation to a point.
-}

module Util.Matrix (matrixPointMultiply) where

import Database.Tables (Matrix, Point)

-- | Apply a matrix transformation to a point.
-- The matrix must have dimensions 3x3, representing a transformation for a two-dimensional point,
-- i.e., the transformation in the z-component is ignored, so the third row is expected to be [0,0,1]
matrixPointMultiply :: Matrix -> Point -> Point
matrixPointMultiply matrix (x, y) =
    case matrix of
        [[a, b, tx], [c, d, ty], _] -> (a * x + b * y + tx, c * x + d * y + ty)
        _ -> error "Matrix must be 3x3 for point transformation."
