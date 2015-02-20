{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module SVGBuilder where

import SVGTypes
import Tables
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Conduit.List as CL
import Database.Persist
import Database.Persist.Sqlite
import Data.Char
import Data.Conduit
import Data.List.Split
import Data.List
import JsonParser
import ParserUtil

-- | Builds a Path from a database entry in the paths table.
buildPaths :: Int -> [Paths] -> [Path]
buildPaths _ [] = [] 
buildPaths idCounter entities = do
    let entity = head entities
    Path ('p' : show idCounter)
         (map point $ pathsD entity)
         (pathsFill entity)
         (pathsFillOpacity entity)
         (pathsStroke entity)
         (pathsIsRegion entity)
         ""
         "" : buildPaths (idCounter + 1) (tail entities)

-- | Builds a Rect from a database entry in the rects table.
buildRect :: [Text] -> Rects -> Rect
buildRect texts entity = do
    let rectTexts = filter (\x -> intersects
                            (fromRational (rectsWidth entity))
                            (fromRational (rectsHeight entity))
                            (fromRational (rectsXPos entity))
                            (fromRational (rectsYPos entity))
                            9
                            (fromRational (textXPos x))
                            (fromRational (textYPos x))
                            ) texts
    let textString = concat $ map textText rectTexts
    let id_ = (if rectsIsHybrid entity then "h" else "") ++ 
              (if isDigit $ head textString then "CSC" else "") ++ dropSlash textString
    Rect id_
         (rectsWidth entity)
         (rectsHeight entity)
         (rectsXPos entity)
         (rectsYPos entity)
         (rectsFill entity)
         (rectsStroke entity)
         (rectsFillOpacity entity)
         (rectsIsHybrid entity)
         rectTexts

-- | Determines the source and target nodes of the path.
processPath :: [Rect] -> [Ellipse] -> Path -> Path
processPath rects ellipses edge = 
    do let coords = points edge
       let xStart = fromRational $ fst $ head coords
       let yStart = fromRational $ snd $ head coords
       let xEnd = fromRational $ fst $ last coords
       let yEnd = fromRational $ snd $ last coords
       let intersectingSourceRect = getIntersectingShape xStart yStart rects
       let intersectingTargetRect = getIntersectingShape xEnd yEnd rects
       let intersectingSourceBool = getIntersectingShape xStart yStart ellipses
       let intersectingTargetBool = getIntersectingShape xEnd yEnd ellipses
       let sourceNode = if null intersectingSourceRect then intersectingSourceBool else intersectingSourceRect
       let targetNode = if null intersectingTargetRect then intersectingTargetBool else intersectingTargetRect
       Path (pathId edge)
            (points edge)
            (pathFill edge)
            (pathFillOpacity edge)
            (pathStroke edge)
            (pathIsRegion edge)
            sourceNode
            targetNode

-- | Gets the first rect that intersects with the given coordinates.
getIntersectingShape :: Shape a => Float -> Float -> [a] -> String
getIntersectingShape xpos ypos shapes = do
    let intersectingShapes = filter (intersectsWithPoint xpos ypos) shapes
    if null intersectingShapes
    then ""
    else getId $ head intersectingShapes

-- | Determines if a rect intersects with the given coordinates.
intersectsWithPoint :: Shape a => Float -> Float -> a -> Bool
intersectsWithPoint xpos ypos shape =
    intersects (getWidth shape)
               (getHeight shape)
               (getX shape)
               (getY shape)
               (getTolerance shape)
               xpos
               ypos

-- | Prints the database table 'rects'.
printDB :: IO ()
printDB = runSqlite dbStr $ do
              let sql = "SELECT * FROM rects"
              rawQuery sql [] $$ CL.mapM_ (liftIO . print)

-- | Builds a Text from a database entry in the texts table.
buildText :: Texts -> Text
buildText entity = 
    Text (textsXPos entity)
         (textsYPos entity)
         (textsText entity)
         (textsFontSize entity)
         (textsFontWeight entity)
         (textsFontFamily entity)

-- | Builds a Path from a database entry in the paths table.
buildEllipses :: [Text] -> Int -> [Ellipses] -> [Ellipse]
buildEllipses _ _ [] = []
buildEllipses texts idCounter entities = do
    let entity = head entities
    let ellipseText = filter (\x -> 
                                  intersects
                                  5
                                  5
                                  (fromRational (ellipsesXPos entity))
                                  (fromRational (ellipsesYPos entity))
                                  9
                                  (fromRational (textXPos x))
                                  (fromRational (textYPos x))
                                  ) texts
    Ellipse ("bool" ++ show idCounter)
            (ellipsesXPos entity)
            (ellipsesYPos entity)
            (ellipsesRx entity)
            (ellipsesRy entity)
            (ellipsesStroke entity)
            ellipseText : buildEllipses texts (idCounter + 1) (tail entities)

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [(Rational, Rational)] -> String
buildPathString d = unwords $ map (joinPathTuple . convertRationalTupToString) d

-- | Joins two String values in a tuple with a comma.
joinPathTuple :: (String, String) -> String
joinPathTuple tup = fst tup ++ "," ++ snd tup

-- | Converts a tuple of Rationals to a tuple of String.
convertRationalTupToString :: (Rational, Rational) -> (String, String)
convertRationalTupToString tup = (show $ fromRational (fst tup), show $ fromRational (snd tup))