{-# LANGUAGE OverloadedStrings #-}

module WebParsing.GraphVisualization
    (makeGraph) where

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Graph.Inductive.Graph
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing
import Data.GraphViz.Commands
import Data.List
import Database.CourseQueries
import Network.HTTP
import System.Process
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import WebParsing.GraphConversion
import WebParsing.ArtSciParser

-- | simple GraphViz parameters for a deparment graph: labels nodes.
graphParams :: GraphvizParams n T.Text el () T.Text
graphParams =
                nonClusteredParams {  globalAttributes = [GraphAttrs [Splines Ortho,
                                                          Ratio FillRatio,

                                                          Size (GSize 8 (Just 4.8) False)],
                                                          NodeAttrs [ Shape BoxShape,
                                                                      FontSize 80,
                                                                      Style [SItem Bold []]],
                                                          EdgeAttrs [Style [SItem Bold []]]],
                                      fmtNode = fn
                                    }
              where fn (_,l) = [toLabel (T.unpack l)]

--takes in a String representation of a dot graph and creates an SVG file.
graphVizProcess :: PrintDotRepr dg n => FilePath  -> dg n -> IO FilePath
graphVizProcess name graph = runGraphvizCommand Dot graph Canon name

-- | outputs an SVG file with name filename containing a graph of nodes in courses
makeGraph' :: [String] -> String -> IO FilePath
makeGraph' codes filename =
    let courses = Prelude.map T.pack codes
    in (toGraph' courses) >>=
       (\g -> graphVizProcess filename (graphToDot graphParams g))

-- | constructs a graph of all courses starting with code.
makeGraph :: String ->  IO ProcessHandle
makeGraph code = do
    getDeptCourses code >>=
      toGraph >>=
       (\g -> graphVizProcess code (graphToDot graphParams g)) >>=
          (\_ -> runCommand $ "unflatten -f -l50 -c 4 -o out.dot " ++ code) >>=
            (\_ -> runCommand $ "dot -Tsvg -o ./graphs/" ++ code ++".svg" ++ " out.dot")


junkCodes :: [String]
junkCodes = ["CMS"]

main :: IO ()
main = do
    rsp <- simpleHTTP (getRequest fasCalendarURL)
    body <- getResponseBody rsp
    let depts = filter (isPrefixOf "crs_")  (getDeptList $ parseTags body)
    let codes = map (drop 4 . takeWhile (/= '.') . map toUpper)  depts\\ junkCodes
    mapM_ makeGraph codes
    mapM_ (\x -> (runCommand $ "rm " ++ (map toUpper x))) codes
