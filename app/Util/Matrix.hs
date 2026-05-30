{-|
    Module      : Util.Matrix
    Description : Contains a single function that applies a matrix transformation to a point.
-}

module Util.Matrix (matrixPointMultiply, matrixMultiply, dotProduct) where

import Database.Tables (Matrix, Point, Vector)
import Data.List (transpose)

-- | Apply a matrix transformation to a point.
-- The matrix must have dimensions 3x3, representing a transformation for a two-dimensional point,
-- i.e., the transformation in the z-component is ignored, so the third row is expected to be [0,0,1]
matrixPointMultiply :: Matrix -> Point -> Point
matrixPointMultiply matrix (x, y) =
    case matrix of
        [[a, b, tx], [c, d, ty], _] -> (a * x + b * y + tx, c * x + d * y + ty)
        _ -> error "Matrix must be 3x3 for point transformation."

-- | Multiplies two 3x3 matrices together.
matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply m1 m2 = [[dotProduct row col | col <- transpose m2] | row <- m1]

-- | Computes the dot product of two vectors.
dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 = sum $ zipWith (*) v1 v2
