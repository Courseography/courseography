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
import qualified Data.Text as T
import Database.DataType
import Database.Tables hiding (shapes, texts)
import Svg.Parser (matrixPointMultiply)
import Util.Helpers

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
          entity {pathId_ = T.concat [pathId_ entity, "p", T.pack $ show elementId],
                  pathSource = "",
                  pathTarget = ""}
    | otherwise =
          let coords = pathPoints entity
              start = safeHead (0.0, 0.0) coords
              end = last coords
              nodes = rects ++ ellipses
              sourceNode =
                  if T.null $ pathSource entity
                      then getIntersectingShape
                          (matrixPointMultiply (listToMatrix $ pathTransform entity) start)
                          nodes
                      else pathSource entity
              targetNode =
                  if T.null $ pathTarget entity
                      then getIntersectingShape
                          (matrixPointMultiply (listToMatrix $ pathTransform entity) end)
                          (filter (\r -> shapeId_ r /= sourceNode) nodes)
                      else pathTarget entity
          in
              entity {pathId_ = T.pack $ 'p' : show elementId,
                      pathSource = sourceNode,
                      pathTarget = targetNode}

-- | Builds a Rect from a database entry.
-- Fills in the text association(s) and ID.
buildRect :: [Text]  -- ^ A list of shapes that may intersect with the given node.
          -> Shape   -- ^ A node.
          -> Integer -- ^ An integer to uniquely identify the shape
          -> Shape
buildRect texts entity elementId =
    let rectTexts = filter
                (\text -> intersects
                    (shapeWidth entity)
                    (shapeHeight entity)
                    (shapePos entity)
                    0  -- no tolerance for text intersection
                    (matrixPointMultiply
                        (invertMatrix3x3 (listToMatrix $ shapeTransform entity))
                        (matrixPointMultiply (listToMatrix $ textTransform text) (textPos text))
                    )
                ) texts
        textString = T.concat $ map textText rectTexts
        id_ = case shapeType_ entity of
              Hybrid -> T.pack $ 'h' : show elementId
              Node -> if shapeId_ entity == ""
                  then T.map toLower . sanitizeId $ textString
                  else shapeId_ entity
              BoolNode -> shapeId_ entity
              Region -> ""
    in
        entity {shapeId_ = id_,
                shapeText = rectTexts}

-- | Builds an ellipse from a database entry.
-- Fills in the text association and ID.
buildEllipses :: [Text]  -- ^ A list of Text elements that may or may not intersect
                         --   with the given ellipse.
              -> Shape   -- ^ An ellipse.
              -> Integer -- ^ A number to use in the ID of the ellipse.
              -> Shape
buildEllipses texts entity elementId =
    let ellipseText = filter
                    (\text -> intersectsEllipse
                        (shapeWidth entity / 2)
                        (shapeHeight entity / 2)
                        (fst (shapePos entity) - shapeWidth entity / 2,
                         snd (shapePos entity) - shapeHeight entity / 2)
                        (matrixPointMultiply
                            (invertMatrix3x3 (listToMatrix $ shapeTransform entity))
                            (matrixPointMultiply (listToMatrix $ textTransform text) (textPos text))
                        )
                    ) texts
    in
        entity {
            shapeId_ =
                if shapeId_ entity == ""
                    then T.pack $ "bool" ++ show elementId
                    else shapeId_ entity,
            shapeText = ellipseText
            }
    where
        intersectsEllipse a b (cx, cy) (x, y) =
            let dx = x - cx - 10  -- some tolerance
                dy = y - cy - 10
            in
                (dx*dx) / (a*a) + (dy*dy) / (b*b) < 1

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [Point] -> T.Text
buildPathString d = T.unwords $ map toString d
    where
        toString (a, b) = T.pack $ show a ++ "," ++ show b


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
    in  dx >= -offset &&
        dx <= width + offset &&
        dy >= -offset &&
        dy <= height + offset;

-- | Determines if a point is contained in a shape.
-- Assumes that the point's transformation has already been applied to point
-- Applies the inverse transformation of the shape to the point before checking for intersection
intersectsWithPoint :: Point -> Shape -> Bool
intersectsWithPoint point shape
    | shapeType_ shape == BoolNode =
        intersects (shapeWidth shape)
                   (shapeHeight shape)
                   (fst (shapePos shape) - shapeWidth shape / 2,
                    snd (shapePos shape) - shapeHeight shape / 2)
                   (shapeTolerance shape)
                   (matrixPointMultiply (invertMatrix3x3 (listToMatrix $ shapeTransform shape)) point)
    | otherwise =
        intersects (shapeWidth shape)
                     (shapeHeight shape)
                     (shapePos shape)
                     (shapeTolerance shape)
                     (matrixPointMultiply (invertMatrix3x3 (listToMatrix $ shapeTransform shape)) point)

-- | Returns the ID of the first shape in a list that intersects
-- with the given point.
getIntersectingShape :: Point -> [Shape] -> T.Text
getIntersectingShape point shapes =
    maybe "" shapeId_ $ find (intersectsWithPoint point) shapes

-- | Determines if a text intersects with any shape in a list.
intersectsWithShape :: [Shape] -> Text -> Bool
intersectsWithShape shapes text =
    any (intersectsWithPoint $ matrixPointMultiply (listToMatrix $ textTransform text) (textPos text)) shapes

-- ** Other helpers

-- | Strips disallowed characters from string for DOM id
sanitizeId :: T.Text -> T.Text
sanitizeId = T.filter (\c -> c `notElem` (",()/<>% " :: String))

-- | Return shape tolerance according to type of shape.
-- BoolNode is with 20.0 tolerance, which Node and Hybrid are with 9.0 tolerance.
shapeTolerance :: Shape -> Double
shapeTolerance s =
  case shapeType_ s of
    BoolNode -> 20.0
    _ -> 9.0


-- * Helpers for matrix inversion

-- Invert a 3x3 matrix. Assumes that the matrix is invertible
invertMatrix3x3 :: Matrix -> Matrix
invertMatrix3x3 m =
    map (map (* (1 / determinantMatrix3x3 m))) (adjointMatrix3x3 m)

-- Calculate the determinant of a 3x3 matrix
determinantMatrix3x3 :: Matrix -> Double
determinantMatrix3x3 [[a, b, c], [d, e, f], [g, h, i]] =
    a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
determinantMatrix3x3 _ = error "Matrix must be 3x3"

-- Caculate the adjoint matrix
adjointMatrix3x3 :: Matrix -> Matrix
adjointMatrix3x3 [[a, b, c], [d, e, f], [g, h, i]] =
    [[ e * i - f * h, -(b * i - c * h),  b * f - c * e],
     [-(d * i - f * g),  a * i - c * g, -(a * f - c * d)],
     [ d * h - e * g, -(a * h - b * g),  a * e - b * d]]
adjointMatrix3x3 _ = error "Matrix must be 3x3"

-- Parse transform back from the format stored in the database
listToMatrix :: [Double] -> Matrix
listToMatrix [a, b, c, d, e, f] = [[a, c, e], [b, d, f], [0, 0, 1]]
listToMatrix _ = error "Expecting 6 values to fully specify a transformation"
