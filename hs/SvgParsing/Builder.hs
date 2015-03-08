{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module SvgParsing.Builder where

import SvgParsing.Types
import SvgParsing.ParserUtil
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Data.Char
import Data.List.Split
import Data.List
import Database.JsonParser
import Database.Tables

-- | Determines the source and target nodes of the path.
buildPath :: [Shape] -> [Shape] -> Path -> Int -> Path
buildPath rects ellipses entity idCounter
    | pathIsRegion entity =
        Path ('p' : show idCounter)
             coords
             (pathFill entity)
             (pathStroke entity)
             (pathIsRegion entity)
             ""
             ""
    | otherwise =
        let (xStart, yStart) = head coords
            (xEnd, yEnd) = last coords
            sourceNode = getIntersectingShape xStart yStart (rects ++ ellipses)
            targetNode = getIntersectingShape xEnd yEnd (rects ++ ellipses)
            in Path ('p' : show idCounter)
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
                            (shapeXPos entity, shapeYPos entity)
                            9
                            (textXPos x, textYPos x)
                            ) texts
        textString = concatMap textText rectTexts
        id_ = map toLower $ (if shapeIsHybrid entity then "h" else "") ++
                            (if isDigit $ head textString then "CSC" else "") ++ dropSlash textString
    in Shape id_
             (shapeXPos entity)
             (shapeYPos entity)
             (shapeWidth entity)
             (shapeHeight entity)
             (shapeFill entity)
             (shapeStroke entity)
             rectTexts
             (shapeIsHybrid entity)
             9
             False

-- | Gets the first rect that intersects with the given coordinates.
getIntersectingShape :: Double -> Double -> [Shape] -> String
getIntersectingShape xpos ypos shapes
    | null intersectingShapes = ""
    | otherwise = shapeId_ $ head intersectingShapes
    where intersectingShapes = filter (intersectsWithPoint xpos ypos)
                                      shapes

-- | Determines if a rect intersects with the given coordinates.
intersectsWithPoint :: Double -> Double -> Shape -> Bool
intersectsWithPoint xpos ypos shape =
    intersects (shapeWidth shape)
               (shapeHeight shape)
               (shapeXPos shape, shapeYPos shape)
               (shapeTolerance shape)
               (xpos, ypos)

-- | Builds a Path from a database entry in the paths table.
buildEllipses :: [Text] -> Int -> [Shape] -> [Shape]
buildEllipses _ _ [] = []
buildEllipses texts idCounter entities =
    let entity = head entities
        ellipseText = filter (\x -> intersects
                                    (shapeWidth entity)
                                    (shapeHeight entity)
                                    (shapeXPos entity,
                                     shapeYPos entity)
                                    9
                                    (textXPos x, textYPos x)
                             ) texts
    in Shape ("bool" ++ show idCounter)
             (shapeXPos entity)
             (shapeYPos entity)
             (shapeWidth entity)
             (shapeHeight entity)
             ""
             (shapeStroke entity)
             ellipseText
             False
             20
             True : buildEllipses texts (idCounter + 1) (tail entities)

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [Point] -> String
buildPathString d = unwords $ map (joinPathTuple . convertRationalTupToString) d

-- | Joins two String values in a tuple with a comma.
joinPathTuple :: (String, String) -> String
joinPathTuple (a, b) = a ++ "," ++ b

-- | Converts a tuple of Rationals to a tuple of String.
convertRationalTupToString :: Point -> (String, String)
convertRationalTupToString (a, b) = (show a, show b)
