{-# LANGUAGE OverloadedStrings #-}

module WebParsing.GraphVisualization (makeGraph) where

import Data.Text
import qualified Data.Text.Lazy as L
import Data.Graph.Inductive.Graph
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing
import Data.GraphViz.Commands
import System.Process
import WebParsing.GraphConversion
import Database.CourseQueries

-- | simple GraphViz parameters for a deparment graph: labels nodes.
graphParams :: GraphvizParams n Text el () Text
graphParams =
                nonClusteredParams {  globalAttributes = [GraphAttrs [Splines Ortho,
                                                          Ratio FillRatio,
                                                          RankSep [1.0],
                                                          Size (GSize 10 (Just 6) False)],
                                                          NodeAttrs [ Shape BoxShape,
                                                                      FontSize 40],
                                                          EdgeAttrs []],
                                      fmtNode = fn
                                    }
              where fn (_,l) = [toLabel (unpack l)]

--takes in a String representation of a dot graph and creates an SVG file.
graphVizProcess :: PrintDotRepr dg n => FilePath  -> dg n -> IO FilePath
graphVizProcess name graph = runGraphvizCommand Dot graph Svg name

-- | outputs an SVG file with name filename containing a graph of nodes in courses
makeGraph' :: [String] -> String -> IO FilePath
makeGraph' codes filename =
  let courses = Prelude.map pack codes
  in (toGraph' courses) >>=
     (\g -> graphVizProcess filename (graphToDot graphParams g))

-- | constructs a graph of all courses starting with code.
makeGraph :: String ->  IO FilePath
makeGraph code = do
  getDepartment code >>=
    toGraph >>=
     (\g -> graphVizProcess code (graphToDot graphParams g))

main :: IO ()
main =
  do
  getDepartment "CSC" >>=
    toGraph >>=
      (\g -> print $ graphToDot graphParams g)