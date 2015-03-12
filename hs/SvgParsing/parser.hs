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
           (shapes, paths, texts) = parseNode False (Style (0,0) "" "") (getRoot graphDoc)
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
       createDirectoryIfMissing True ("../res/graphs/" ++ dirLocation)
       buildSVG M.empty ("../res/graphs/" ++ dirLocation ++ "/" ++ outputFilename)
       print "SVG Built"

-- | Parses a level.
parseNode :: Bool -> Style -> Content i -> ([Path],[Shape],[Text])
parseNode currentlyInRegion style content =
    if getAttribute "id" content == "layer2" ||
       getName content == "defs"
    then ([],[],[])
    else
        let isRegion       = getAttribute "id" content == "layer3"
            newTransform   = getAttribute "transform" content
            newStyle       = getAttribute "style" content
            newFill        = getNewStyleAttr newStyle "fill" (fill style)
            newStroke      = getNewStyleAttr newStyle "stroke" (stroke style)
            x = if null newTransform then (0,0) else parseTransform newTransform
            adjustedTransform = addTuples (transform style) x
            parentStyle = Style adjustedTransform
                                newFill
                                newStroke
            x1 = map (parseRect parentStyle) (tag "rect" content)
            x2 = map (parseText parentStyle) (tag "text" content)
            x3 = map (updatePathTransform adjustedTransform) $ mapMaybe (parsePath (currentlyInRegion || isRegion) parentStyle) (tag "path" content)
            x4 = map (parseEllipse parentStyle) (tag "ellipse" content)
        in
            addThree (x3,x1++x4,x2) $ parseChildren (currentlyInRegion || isRegion)
                                                    parentStyle
                                                    (getChildren content)

-- | Parses a list of Content.
parseChildren :: Bool -> Style -> [Content i] -> ([Path],[Shape],[Text])
parseChildren currentlyInRegion style x =
     foldl addThree ([],[],[]) $ map (parseNode currentlyInRegion style) x


-- TODO: Can't find way to zip tuples.
addThree :: ([Path],[Shape],[Text])
         -> ([Path],[Shape],[Text])
         -> ([Path],[Shape],[Text])
addThree (a,b,c) (d,e,f) = (a ++ d, b ++ e, c ++f)

-- | Parses a rect.
parseRect :: Style -> Content i -> Shape
parseRect style content =
    Shape 1
          ""
          (read (getAttribute "x" content) + fst (transform style),
           read (getAttribute "y" content) + snd (transform style))
          (read $ getAttribute "width" content)
          (read $ getAttribute "height" content)
          (fill style)
          (stroke style)
          []
          (fill style == "#a14c3a")
          9
          False

-- | Parses a path.
parsePath :: Bool -> Style -> Content i -> Maybe Path
parsePath isRegion style content =
    if last (getAttribute "d" content) == 'z' && not isRegion
    then Nothing
    else Just (Path 1
                    "p"
                    d
                    (fill style)
                    (stroke style)
                    isRegion
                    ""
                    "")
    where d = parsePathD $ getAttribute "d" content

-- | Parses a text.
parseText :: Style -> Content i -> Text
parseText style content =
    Text 1
         (getAttribute "id" content)
         (read (getAttribute "x" content) + fst (transform style),
          read (getAttribute "y" content) + snd (transform style))
         (tagTextContent content)

-- | Parses a text.
parseEllipse :: Style -> Content i -> Shape
parseEllipse style content =
    Shape 1
          ""
          (read (getAttribute "cx" content) + fst (transform style),
           read (getAttribute "cy" content) + snd (transform style))
          (read (getAttribute "rx" content) * 2)
          (read (getAttribute "ry" content) * 2)
          ""
          (stroke style)
          []
          False
          20
          True

updatePathTransform :: Point -> Path -> Path
updatePathTransform transform p = p { pathPoints = map (addTuples transform) (pathPoints p)}