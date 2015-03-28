{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module SvgParsing.Builder where

import SvgParsing.ParserUtil
import Database.Persist
import Database.Persist.Sqlite
import Data.Char
import Data.List.Split
import Data.List
import Database.JsonParser
import Database.Tables
import Database.DataType

-- | Determines the source and target nodes of the path.
buildPath :: [Shape] -> [Shape] -> Path -> Int -> Path
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
            targetNode = getIntersectingShape end (rects ++ ellipses)
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
buildRect :: [Text] -> Shape -> Shape
buildRect texts entity =
    let rectTexts = filter (\x -> intersects
                            (shapeWidth entity)
                            (shapeHeight entity)
                            (shapePos entity)
                            9
                            (textPos x)
                            ) texts
        textString = concatMap textText rectTexts
        dropSlash = takeWhile (/='/')
        prefix = case shapeType_ entity of
                     Hybrid -> "h"
                     _      -> ""
        id_ = map toLower (prefix ++
                          (if isDigit $ head textString then "CSC" else "") ++
                          dropSlash textString)
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
getIntersectingShape point shapes
    | null intersectingShapes = ""
    | otherwise = shapeId_ $ head intersectingShapes
    where intersectingShapes = filter (intersectsWithPoint point)
                                      shapes

-- | Determines if a rect intersects with the given coordinates.
intersectsWithPoint :: Point -> Shape -> Bool
intersectsWithPoint point shape =
    intersects (shapeWidth shape)
               (shapeHeight shape)
               (shapePos shape)
               (shapeTolerance shape)
               point

-- | Builds a Path from a database entry in the paths table.
buildEllipses :: [Text] -> Int -> [Shape] -> [Shape]
buildEllipses _ _ [] = []
buildEllipses texts idCounter entities =
    let entity = head entities
        ellipseText = filter (\x -> intersects
                                    (shapeWidth entity)
                                    (shapeHeight entity)
                                    (shapePos entity)
                                    9
                                    (textPos x)
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
             (shapeType_ entity) : buildEllipses texts (idCounter + 1) (tail entities)

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [Point] -> String
buildPathString d = unwords $ map (joinPathTuple . convertRationalTupToString) d

-- | Joins two String values in a tuple with a comma.
joinPathTuple :: (String, String) -> String
joinPathTuple (a, b) = a ++ "," ++ b

-- | Converts a tuple of Rationals to a tuple of String.
convertRationalTupToString :: Point -> (String, String)
convertRationalTupToString (a, b) = (show a, show b)
