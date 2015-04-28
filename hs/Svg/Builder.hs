{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}

-- |This module takes the raw data parsed from the SVG files and computes
--  prerequisite relationships based on the geometry.
--
--  This is currently done after the data is first inserted, when the new
--  SVG is generated. This work should really be done immediately after
--  parsing, before anything is inserted into the database.
module Svg.Builder where

import Svg.ParserUtil
import Data.Char
import Data.List
import Database.Tables
import Database.DataType
import Data.Int (Int64)

-- | Determines the source and target nodes of the path.
buildPath :: [Shape] -- ^ Node elements.
          -> [Shape] -- ^ Ellipses.
          -> Path    -- ^ A path.
          -> Int64   -- ^ A number to use in the ID of the path.
          -> Path
buildPath rects ellipses entity idCounter
    | pathIsRegion entity =
        Path (pathGId entity)
             (pathId_ entity ++ ('p' : show idCounter))
             coords
             (pathFill entity)
             (pathStroke entity)
             (pathIsRegion entity)
             ""
             ""
    | otherwise =
        let start = head coords
            end = last coords
            sourceNode = getIntersectingShape start (rects ++ ellipses)
            targetNode = getIntersectingShape end
                             (filter (\r -> shapeId_ r /= sourceNode) rects ++ ellipses)
            in Path (pathGId entity)
                    ('p' : show idCounter)
                    coords
                    (pathFill entity)
                    (pathStroke entity)
                    (pathIsRegion entity)
                    sourceNode
                    targetNode
    where coords = pathPoints entity

-- | Builds a Rect from a database entry in the rects table.
buildRect :: [Text] -- ^ A list of shapes that may intersect with the given node.
          -> Shape  -- ^ A node.
          -> Int64    -- ^ An integer to uniquely identify the shape
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
    in Shape (shapeGId entity)
             id_
             (shapePos entity)
             (shapeWidth entity)
             (shapeHeight entity)
             (shapeFill entity)
             (shapeStroke entity)
             rectTexts
             9
             (shapeType_ entity)

-- | Gets the first rect that intersects with the given coordinates.
getIntersectingShape :: Point -> [Shape] -> String
getIntersectingShape point shapes =
    case find (intersectsWithPoint point) shapes of
        Just intersectingShape -> shapeId_ intersectingShape
        _                      -> ""

-- | Determines if a rect intersects with the given coordinates.
intersectsWithPoint :: Point -> Shape -> Bool
intersectsWithPoint point shape =
    intersects (shapeWidth shape)
               (shapeHeight shape)
               (shapePos shape)
               (shapeTolerance shape)
               point

-- | Builds a Path from a database entry in the paths table.
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
                              9
                              . textPos
                              ) texts
    in Shape (shapeGId entity)
             ("bool" ++ show idCounter)
             (shapePos entity)
             (shapeWidth entity)
             (shapeHeight entity)
             ""
             (shapeStroke entity)
             ellipseText
             20
             (shapeType_ entity)

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [Point] -> String
buildPathString d = unwords $ map (joinPathTuple . convertRationalTupToString) d

-- | Joins two String values in a tuple with a comma.
joinPathTuple :: (String, String) -> String
joinPathTuple (a, b) = a ++ "," ++ b

-- | Converts a tuple of Rationals to a tuple of String.
convertRationalTupToString :: Point -> (String, String)
convertRationalTupToString (a, b) = (show a, show b)
