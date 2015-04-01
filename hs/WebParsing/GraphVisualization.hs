{-# LANGUAGE OverloadedStrings #-}

module WebParsing.GraphVisualization (makeGraph) where

import Data.Text
import qualified Data.Text.Lazy as L
import Data.Graph.Inductive.Graph
import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Commands
import System.Process
import WebParsing.GraphConversion

-- | simple GraphViz parameters for a deparment graph: labels nodes.
graphParams :: GraphvizParams n Text el () Text
graphParams = nonClusteredParams {fmtNode = fn}
              where fn (_,l) = [toLabel (unpack l)]
              
--takes in a String representation of a dot graph and creates an SVG file.
graphVizProcess :: PrintDotRepr dg n => FilePath  -> dg n -> IO FilePath
graphVizProcess name graph = runGraphvizCommand Dot graph Svg name

-- | outputs an SVG file with name filename containing a graph of nodes in courses
makeGraph :: [String] -> String -> IO FilePath
makeGraph courses filename =
  let courses' = Prelude.map pack courses
  in (toGraph courses') >>= 
     (\g -> graphVizProcess filename (graphToDot graphParams g)) 

main :: IO FilePath
main = 
  compsci >>= 
  (\x -> return (graphToDot graphParams x)) >>=
  graphVizProcess "compsci"



