{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module Main where

import Text.XML.HaXml (Content, path, tag, children, xmlParse)
import Text.XML.HaXml.Util (tagTextContent)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import Data.Int
import Database.Tables
import Database.DataType
import Database.SvgDatabase
import SvgParsing.Generator
import SvgParsing.ParserUtil
import qualified Data.Map as M (empty)

main :: IO ()
main = performParse "CSC" "graph_regions.svg"

performParse :: String -- ^ The title of the graph.
             -> String -- ^ The filename of the file that will be parsed.
             -> IO ()
performParse graphTitle inputFilename =
   do graphFile <- readFile ("../public/res/graphs/" ++ inputFilename)
      key <- insertGraph graphTitle
      let parsedGraph = parseGraph key graphFile
      print "Graph Parsed"
      insertElements parsedGraph
      print "Graph Inserted"
      buildSVG 1 M.empty ("../public/res/graphs/" ++ show 1 ++ ".svg") False
      print "Success"

parseGraph ::  Int64  -- ^ The unique identifier of the graph.
            -> String -- ^ The file contents of the graph that will be parsed.
            -> ([Path],[Shape],[Text])
parseGraph key graphFile =
    let graphDoc = xmlParse "output.error" graphFile
    in parseNode key (getRoot graphDoc)

-- | Parses a level.
parseNode :: Int64 -- ^ The Path's corresponding graph identifier.
          -> Content i
          -> ([Path],[Shape],[Text])
parseNode key content =
    if getAttribute "id" content == "layer2" ||
       getName content == "defs"
    then ([],[],[])
    else let trans = parseTransform $ getAttribute "transform" content
             style = getAttribute "style" content
             fill = getStyleAttr "fill" style
             (chilrenPaths, childrenShapes, childrenTexts) =
                 parseChildren key (path [children] content)
             rects = map (parseRect key) (tag "rect" content)
             texts = map (parseText key) (tag "text" content)
             paths = mapMaybe (parsePath key) (tag "path" content)
             ellipses = map (parseEllipse key) (tag "ellipse" content)
         in (map (updatePath fill trans) (paths ++ chilrenPaths),
             map (updateShape fill trans) (rects ++ ellipses ++ childrenShapes),
             map (updateText trans) (texts ++ childrenTexts))

-- | Parses a list of Content.
parseChildren :: Int64 -- ^ The corresponding graph identifier.
              -> [Content i]
              -> ([Path],[Shape],[Text])
parseChildren key x = foldl addThree ([],[],[]) $ map (parseNode key) x

-- TODO: Can't find way to zip tuples.
addThree :: ([Path],[Shape],[Text])
         -> ([Path],[Shape],[Text])
         -> ([Path],[Shape],[Text])
addThree (a,b,c) (d,e,f) = (a ++ d, b ++ e, c ++f)

-- | Parses a rect.
parseRect :: Int64 -- ^ The Rect's corresponding graph identifier.
          -> Content i
          -> Shape
parseRect key content =
    Shape key
          ""
          (readAttr "x" content,
           readAttr "y" content)
          (readAttr "width" content)
          (readAttr "height" content)
          ""
          ""
          []
          9
          Node

-- | Parses a path.
parsePath :: Int64 -- ^ The Path's corresponding graph identifier.
          -> Content i
          -> Maybe Path
parsePath key content =
    if last (getAttribute "d" content) == 'z' && not isRegion
    then Nothing
    else Just (Path key
                    ""
                    d
                    ""
                    ""
                    isRegion
                    ""
                    "")
    where d = parsePathD $ getAttribute "d" content
          fillAttr = getStyleAttr "fill" (getAttribute "style" content)
          isRegion = not $
              null fillAttr || fillAttr == "none"

-- | Parses a text.
parseText :: Int64 -- ^ The Text's corresponding graph identifier.
          -> Content i
          -> Text
parseText key content =
    Text key
         (getAttribute "id" content)
         (readAttr "x" content,
          readAttr "y" content)
         (tagTextContent content)

-- | Parses an ellipse.
parseEllipse :: Int64 -- ^ The Ellipse's corresponding graph identifier.
             -> Content i
             -> Shape
parseEllipse key content =
    Shape key
          ""
          (readAttr "cx" content,
           readAttr "cy" content)
          (readAttr "rx" content * 2)
          (readAttr "ry" content * 2)
          ""
          ""
          []
          20
          BoolNode

updatePath :: String -- ^ The fill that may be added to the Path.
           -> Point  -- ^ Transform that will be added to the Shape's
                     --   current transform value.
           -> Path
           -> Path
updatePath fill transform p =
    p { pathPoints = map (addTuples transform) (pathPoints p),
        pathFill = if null (pathFill p) then fill else pathFill p
      }

updateShape :: String -- ^ The fill that may be added to the Shape.
            -> Point  -- ^ Transform that will be added to the Shape's
                      --   current transform value.
            -> Shape
            -> Shape
updateShape fill transform r =
    r { shapePos = addTuples transform (shapePos r),
        shapeFill = if null (shapeFill r) then fill else shapeFill r,
        shapeType_ = if fill == "#a14c3a" then Hybrid
                     else case shapeType_ r of
                              Hybrid   -> Hybrid
                              BoolNode -> BoolNode
                              Node     -> Node
      }

updateText :: Point -- ^ Transform that will be added to the input Shape's
                    --   current transform value.
           -> Text
           -> Text
updateText transform t =
    t { textPos = addTuples transform (textPos t) }
