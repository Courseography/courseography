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
import Database.JsonParser
import Database.SvgDatabase
import SvgParsing.Generator
import SvgParsing.Builder
import SvgParsing.Types
import SvgParsing.ParserUtil
import qualified Data.Map as M

main :: IO ()
main = parseGraph "CSC"
                  "graph_regions.svg"
                  "csc_graph.svg"

parseGraph :: String -> String -> String -> IO ()
parseGraph dirLocation inputFilename outputFilename =
    do graphFile <- readFile ("../res/graphs/" ++ inputFilename)
       print "Parsing SVG file..."
       let graphDoc = xmlParse "output.error" graphFile
           (shapes, paths, texts) = parseNode False (getRoot graphDoc)
       print "Parsing complete"
       runSqlite dbStr $ do
           runMigration migrateAll
           deleteWhere [GraphGId ==. 1]
           deleteWhere [ShapeGId ==. 1]
           deleteWhere [PathGId  ==. 1]
           deleteWhere [TextGId  ==. 1]
           insert_ $ Graph 1 dirLocation
           mapM_ insert_ shapes
           mapM_ insert_ paths
           mapM_ insert_ texts
       print "SVG Inserted"
       createDirectoryIfMissing True ("../res/graphs/" ++ dirLocation)
       buildSVG M.empty ("../res/graphs/" ++ dirLocation ++ "/" ++ outputFilename)
       print "SVG Built"

-- | Parses a level.
parseNode :: Bool -> Content i -> ([Path],[Shape],[Text])
parseNode currentlyInRegion content =
    if getAttribute "id" content == "layer2" ||
       getName content == "defs"
    then ([],[],[])
    else
        let isRegion       = getAttribute "id" content == "layer3"
            trans          = parseTransform $ getAttribute "transform" content
            style          = getAttribute "style" content
            fill           = getNewStyleAttr newStyle "fill" ""
            (chilrenPaths, childrenShapes, childrenTexts) = parseChildren (currentlyInRegion || isRegion) (getChildren content)

            rects    = map ((updateShape newFill trans) . parseRect) (tag "rect" content)
            texts    = map ((updateText trans) . parseText) (tag "text" content)
            paths    = map (updatePath fill trans) $ mapMaybe (parsePath (currentlyInRegion || isRegion)) (tag "path" content)
            ellipses = map ((updateShape fill trans) . parseEllipse) (tag "ellipse" content)
        in
            addThree (paths, rects ++ ellipses, texts) $
             ((map (updatePath fill trans) chilrenPaths),
              (map (updateShape fill trans) childrenShapes),
              (map (updateText trans) childrenTexts))

-- | Parses a list of Content.
parseChildren :: Bool -> [Content i] -> ([Path],[Shape],[Text])
parseChildren currentlyInRegion x =
     foldl addThree ([],[],[]) $ map (parseNode currentlyInRegion) x


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
          (read (getAttribute "x" content),
           read (getAttribute "y" content))
          (read $ getAttribute "width" content)
          (read $ getAttribute "height" content)
          ""
          ""
          []
          False
          9
          False

-- | Parses a path.
parsePath :: Bool -> Content i -> Maybe Path
parsePath isRegion content =
    if last (getAttribute "d" content) == 'z' && not isRegion
    then Nothing
    else Just (Path 1
                    "p"
                    d
                    ""
                    ""
                    isRegion
                    ""
                    "")
    where d = parsePathD $ getAttribute "d" content

-- | Parses a text.
parseText :: Content i -> Text
parseText content =
    Text 1
         (getAttribute "id" content)
         (read (getAttribute "x" content),
          read (getAttribute "y" content))
         (tagTextContent content)

-- | Parses a text.
parseEllipse :: Content i -> Shape
parseEllipse content =
    Shape 1
          ""
          (read (getAttribute "cx" content),
           read (getAttribute "cy" content))
          (read (getAttribute "rx" content) * 2)
          (read (getAttribute "ry" content) * 2)
          ""
          ""
          []
          False
          20
          True

updatePath :: String -> Point -> Path -> Path
updatePath fill transform p =
    p { pathPoints = map (addTuples transform) (pathPoints p),
        pathFill = if null (pathFill p) then fill else pathFill p }

updateShape :: String -> Point -> Shape -> Shape
updateShape fill transform r =
    r { shapePos = addTuples transform (shapePos r),
        shapeFill = if null (shapeFill r) then fill else shapeFill r,
        shapeIsHybrid = shapeIsHybrid r || fill == "#a14c3a" }

updateText :: Point -> Text -> Text
updateText transform r =
    r { textPos = addTuples transform (textPos r) }
