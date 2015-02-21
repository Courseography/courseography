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

-- | Determines the source and target nodes of the path.
buildPath :: [Shape] -> [Shape] -> Paths -> Int -> Path
buildPath rects ellipses entity idCounter =
    let coords = map point $ pathsD entity in
    if pathsIsRegion entity then
        Path ('p' : show idCounter)
             coords
             (pathsFill entity)
             (pathsFillOpacity entity)
             (pathsStroke entity)
             (pathsIsRegion entity)
             ""
             ""
    else
    let xStart = fst $ head coords
        yStart = snd $ head coords
        xEnd = fst $ last coords
        yEnd = snd $ last coords
        sourceNode = getIntersectingShape xStart yStart (rects ++ ellipses)
        targetNode = getIntersectingShape xEnd yEnd (rects ++ ellipses) in
        Path ('p' : show idCounter)
             coords
             (pathsFill entity)
             (pathsFillOpacity entity)
             (pathsStroke entity)
             (pathsIsRegion entity)
             sourceNode
             targetNode

-- | Builds a Rect from a database entry in the rects table.
buildRect :: [Text] -> Rects -> Shape
buildRect texts entity = do
    let rectTexts = filter (\x -> intersects
                            (rectsWidth entity)
                            (rectsHeight entity)
                            (rectsXPos entity, rectsYPos entity)
                            9
                            (textXPos x, textYPos x)
                            ) texts
    let textString = concat $ map textText rectTexts
    let id_ = (if rectsIsHybrid entity then "h" else "") ++ 
              (if isDigit $ head textString then "CSC" else "") ++ dropSlash textString
    Shape id_
          (rectsXPos entity)
          (rectsYPos entity)
          (rectsWidth entity)
          (rectsHeight entity)
          (rectsFill entity)
          (rectsStroke entity)
          rectTexts
          (rectsIsHybrid entity)
          9

-- | Gets the first rect that intersects with the given coordinates.
getIntersectingShape :: Rational -> Rational -> [Shape] -> String
getIntersectingShape xpos ypos shapes = do
    let intersectingShapes = filter (intersectsWithPoint xpos ypos) shapes
    if null intersectingShapes
    then ""
    else shapeId $ head intersectingShapes

-- | Determines if a rect intersects with the given coordinates.
intersectsWithPoint :: Rational -> Rational -> Shape -> Bool
intersectsWithPoint xpos ypos shape =
    intersects (shapeWidth shape)
               (shapeHeight shape)
               (shapeXPos shape, shapeYPos shape)
               (shapeTolerance shape)
               (xpos, ypos)

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
buildEllipses :: [Text] -> Int -> [Ellipses] -> [Shape]
buildEllipses _ _ [] = []
buildEllipses texts idCounter entities = do
    let entity = head entities
    let ellipseText = filter (\x -> 
                                  intersects
                                  ((ellipsesRx entity) * 2)
                                  ((ellipsesRy entity) * 2)
                                  (ellipsesXPos entity, ellipsesYPos entity)
                                  9
                                  (textXPos x, textYPos x)
                                  ) texts
    Shape ("bool" ++ show idCounter)
            (ellipsesXPos entity)
            (ellipsesYPos entity)
            ((ellipsesRx entity) * 2)
            ((ellipsesRy entity) * 2)
            ""
            (ellipsesStroke entity)
            ellipseText
            False
            20 : buildEllipses texts (idCounter + 1) (tail entities)

-- | Rebuilds a path's `d` attribute based on a list of Rational tuples.
buildPathString :: [(Rational, Rational)] -> String
buildPathString d = unwords $ map (joinPathTuple . convertRationalTupToString) d

-- | Joins two String values in a tuple with a comma.
joinPathTuple :: (String, String) -> String
joinPathTuple tup = fst tup ++ "," ++ snd tup

-- | Converts a tuple of Rationals to a tuple of String.
convertRationalTupToString :: (Rational, Rational) -> (String, String)
convertRationalTupToString tup = (show $ fromRational (fst tup), show $ fromRational (snd tup))