{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs, ScopedTypeVariables #-}
module Main where

import Text.XML.HaXml
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Wrappers
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Util
import Text.XML.HaXml.XmlContent.Parser
import qualified Data.Conduit.List as CL
import Database.Persist
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite
import Control.Monad
import Control.Monad.Trans.Reader
import Text.XML.HaXml.Namespaces
import System.Directory
import Data.Conduit
import Data.List.Split
import Data.Maybe
import Data.List
import Data.Text as T (pack, unpack)
import Database.Tables
import Database.DataType
import Database.JsonParser
import Database.SvgDatabase
import SvgParsing.Generator
import SvgParsing.Builder
import SvgParsing.ParserUtil
import qualified Data.Map as M

main :: IO ()
main = performParse "CSC" "graph_regions.svg" "csc_graph.svg"

performParse :: String -> String -> String -> IO ()
performParse dirLocation inputFilename outputFilename =
   do graphFile <- readFile ("../public/res/graphs/" ++ inputFilename)
      let parsedGraph = parseGraph graphFile
      print "Graph Parsed"
      insertGraph 1 dirLocation parsedGraph
      print "Graph Inserted"
      createDirectoryIfMissing True ("../public/res/graphs/" ++ dirLocation)
      buildSVG 1 M.empty ("../public/res/graphs/" ++ dirLocation ++ "/" ++ outputFilename)
      print "Success"

parseGraph :: String -> ([Path],[Shape],[Text])
parseGraph graphFile =
    let graphDoc = xmlParse "output.error" graphFile
    in parseNode (getRoot graphDoc)

insertGraph :: Int -> String -> ([Path],[Shape],[Text]) -> IO ()
insertGraph gId graphTitle (paths, shapes, texts) =
    runSqlite dbStr $ do
        runMigration migrateAll
        deleteWhere [GraphGId ==. gId]
        deleteWhere [ShapeGId ==. gId]
        deleteWhere [PathGId  ==. gId]
        deleteWhere [TextGId  ==. gId]
        insert_ (Graph gId graphTitle)
        mapM_ insert_ shapes
        mapM_ insert_ paths
        mapM_ insert_ texts

-- | Parses a level.
parseNode :: Content i -> ([Path],[Shape],[Text])
parseNode content =
    if getAttribute "id" content == "layer2" ||
       getName content == "defs"
    then ([],[],[])
    else let trans          = parseTransform $ getAttribute "transform" content
             style          = getAttribute "style" content
             fill           = getStyleAttr "fill" style
             (chilrenPaths, childrenShapes, childrenTexts) = parseChildren (path [children] content)
             rects    = map parseRect (tag "rect" content)
             texts    = map parseText (tag "text" content)
             paths    = mapMaybe parsePath (tag "path" content)
             ellipses = map parseEllipse (tag "ellipse" content)
         in ((map (updatePath fill trans) (paths ++ chilrenPaths)),
             (map (updateShape fill trans) (rects ++ ellipses ++ childrenShapes)),
             (map (updateText trans) (texts ++ childrenTexts)))

-- | Parses a list of Content.
parseChildren :: [Content i] -> ([Path],[Shape],[Text])
parseChildren x = foldl addThree ([],[],[]) $ map parseNode x

-- TODO: Can't find way to zip tuples.
addThree :: ([Path],[Shape],[Text])
         -> ([Path],[Shape],[Text])
         -> ([Path],[Shape],[Text])
addThree (a,b,c) (d,e,f) = (a ++ d, b ++ e, c ++f)

-- | Parses a rect.
parseRect :: Content i -> Shape
parseRect content =
    Shape 1
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
parsePath :: Content i -> Maybe Path
parsePath content =
    if last (getAttribute "d" content) == 'z' && not isRegion
    then Nothing
    else Just (Path 1
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
parseText :: Content i -> Text
parseText content =
    Text 1
         (getAttribute "id" content)
         (readAttr "x" content,
          readAttr "y" content)
         (tagTextContent content)

-- | Parses an ellipse.
parseEllipse :: Content i -> Shape
parseEllipse content =
    Shape 1
          ""
          (readAttr "cx" content,
           readAttr "cy" content)
          ((readAttr "rx" content) * 2)
          ((readAttr "ry" content) * 2)
          ""
          ""
          []
          20
          BoolNode

updatePath :: String -> Point -> Path -> Path
updatePath fill transform p =
    p { pathPoints = map (addTuples transform) (pathPoints p),
        pathFill = if null (pathFill p) then fill else pathFill p
      }

updateShape :: String -> Point -> Shape -> Shape
updateShape fill transform r =
    r { shapePos = addTuples transform (shapePos r),
        shapeFill = if null (shapeFill r) then fill else shapeFill r,
        shapeType_ = if fill == "#a14c3a" then Hybrid
                     else case shapeType_ r of
                              Hybrid   -> Hybrid
                              BoolNode -> BoolNode
                              Node     -> Node
      }

updateText :: Point -> Text -> Text
updateText transform t =
    t { textPos = addTuples transform (textPos t) }
