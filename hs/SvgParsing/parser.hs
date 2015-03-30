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
import Data.List hiding (insert)
import Data.Text as T (pack, unpack)
import Data.Int
import Database.Tables
import Database.DataType
import Database.JsonParser
import Database.SvgDatabase
import SvgParsing.Generator
import SvgParsing.Builder
import SvgParsing.ParserUtil
import qualified Data.Map as M

main :: IO ()
main = performParse "CSC" "graph_regions.svg"

performParse :: String -> String -> IO ()
performParse graphTitle inputFilename =
   do graphFile <- readFile ("../public/res/graphs/" ++ inputFilename)
      key <- insertGraph graphTitle
      let parsedGraph = parseGraph key graphFile
      print "Graph Parsed"
      insertElements parsedGraph
      print "Graph Inserted"
      buildSVG key M.empty ("../public/res/graphs/" ++ show key ++ ".svg")
      print "Success"

parseGraph ::  Int64 -> String -> ([Path],[Shape],[Text])
parseGraph key graphFile =
    let graphDoc = xmlParse "output.error" graphFile
    in parseNode key (getRoot graphDoc)

insertGraph :: String -> IO Int64
insertGraph graphTitle =
    runSqlite dbStr $ do
        runMigration migrateAll
        key <- insert (Graph 0 graphTitle)
        let (PersistInt64 keyId) = toPersistValue key
        update key [GraphGId =. keyId]
        return keyId

insertElements :: ([Path], [Shape], [Text]) -> IO ()
insertElements (paths, shapes, texts) =
    runSqlite dbStr $ do
        mapM_ insert_ shapes
        mapM_ insert_ paths
        mapM_ insert_ texts

-- | Parses a level.
parseNode :: Int64 -> Content i -> ([Path],[Shape],[Text])
parseNode key content =
    if getAttribute "id" content == "layer2" ||
       getName content == "defs"
    then ([],[],[])
    else let trans          = parseTransform $ getAttribute "transform" content
             style          = getAttribute "style" content
             fill           = getNewStyleAttr style "fill" ""
             (chilrenPaths, childrenShapes, childrenTexts) = parseChildren key (path [children] content)
             rects    = map (parseRect key) (tag "rect" content)
             texts    = map (parseText key) (tag "text" content)
             paths    = mapMaybe (parsePath key) (tag "path" content)
             ellipses = map (parseEllipse key) (tag "ellipse" content)
         in ((map (updatePath fill trans) (paths ++ chilrenPaths)),
             (map (updateShape fill trans) (rects ++ ellipses ++ childrenShapes)),
             (map (updateText trans) (texts ++ childrenTexts)))

-- | Parses a list of Content.
parseChildren :: Int64 -> [Content i] -> ([Path],[Shape],[Text])
parseChildren key x = foldl addThree ([],[],[]) $ map (parseNode key) x

-- TODO: Can't find way to zip tuples.
addThree :: ([Path],[Shape],[Text])
         -> ([Path],[Shape],[Text])
         -> ([Path],[Shape],[Text])
addThree (a,b,c) (d,e,f) = (a ++ d, b ++ e, c ++f)

-- | Parses a rect.
parseRect :: Int64 -> Content i -> Shape
parseRect key content =
    Shape key
          ""
          (read $ getAttribute "x" content,
           read $ getAttribute "y" content)
          (read $ getAttribute "width" content)
          (read $ getAttribute "height" content)
          ""
          ""
          []
          9
          Node

-- | Parses a path.
parsePath :: Int64 -> Content i -> Maybe Path
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
          fillAttr = getNewStyleAttr (getAttribute "style" content) "fill" ""
          isRegion = not $
              null fillAttr || fillAttr == "none"

-- | Parses a text.
parseText :: Int64 -> Content i -> Text
parseText key content =
    Text key
         (getAttribute "id" content)
         (read $ getAttribute "x" content,
          read $ getAttribute "y" content)
         (tagTextContent content)

-- | Parses an ellipse.
parseEllipse :: Int64 -> Content i -> Shape
parseEllipse key content =
    Shape key
          ""
          (read $ getAttribute "cx" content,
           read $ getAttribute "cy" content)
          (read (getAttribute "rx" content) * 2)
          (read (getAttribute "ry" content) * 2)
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
