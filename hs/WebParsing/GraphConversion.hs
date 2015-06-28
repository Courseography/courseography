{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : WebParsing.GraphConversion
Description : Converts Course records into Dynamic Inductive Graphs for further modification
Stability   : experimental

Currently has the ability to extract course information from the database and convert it into a
a extendable inductive graph using the DynGraph Class. An inductive graph is a representation of
the graph ADT that is more condusive to manipulation using functional paradigms. More information
can be found here.

DynGraphs are supported by the GraphViz library.
-}
module WebParsing.GraphConversion
    (toGraph, toGraph', compsci) where

import Control.Monad.State
import qualified Data.Text as T
import Data.Graph
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray
import Data.List
import Data.Maybe
import Database.CourseQueries
import Database.Tables
import WebParsing.ParsingHelp
import System.Random
import System.IO.Unsafe

{- Signatures:

-- type definitions
type GraphPart = (Gr T.Text (), NodeMap T.Text)

-- null data types
emptyGr :: Gr T.Text ()
emptyMap :: NodeMap T.Text
emptyEdges :: [(T.Text, T.Text, ())]

-- graph querying
nodeInGraph :: T.Text -> Gr T.Text () -> Bool
edgeInGraph :: (T.Text, T.Text, ()) -> Gr T.Text () -> Bool
edgesInGraph :: [(T.Text, T.Text, ())] -> Gr T.Text () -> [(T.Text, T.Text, ())]

-- graph manipulation
removeEdges :: [(T.Text, T.Text, ())]  -> [(T.Text, T.Text, ())] -> [(T.Text, T.Text, ())]
insMapNode' ::  T.Text -> (Gr T.Text (), NodeMap T.Text) -> (Gr T.Text (), NodeMap T.Text)
insMapNodes' :: [T.Text] -> (Gr T.Text (), NodeMap T.Text) -> (Gr T.Text (), NodeMap T.Text)
insMapEdges' :: [(T.Text, T.Text, ())] -> (Gr T.Text (), NodeMap T.Text) -> (Gr T.Text (), NodeMap T.Text)
insMapEdge' :: (T.Text, T.Text, ()) -> (Gr T.Text (), NodeMap T.Text) -> (Gr T.Text (), NodeMap T.Text)

-- Course Record Conversion
extractAllCourses :: [T.Text] -> [IO Course]
parseCoursesEdges :: [Course] -> GraphPart ->  GraphPart
parseCourseEdge :: GraphPart -> Course -> GraphPart
insertPrereq ::  T.Text -> GraphPart -> [T.Text] -> GraphPart
-}

type GraphPart = (Gr T.Text (), NodeMap T.Text)

-- | an empty graph
emptyGr :: Gr T.Text ()
emptyGr = empty

-- | an empty NodeMap
emptyMap :: NodeMap T.Text
emptyMap = new

-- | an empty list of edges
emptyEdges :: [(T.Text, T.Text, ())]
emptyEdges = []

-- | returns true if a node with label node is in graph
nodeInGraph :: T.Text -> Gr T.Text () -> Bool
nodeInGraph node graph =
    foldl' (\acc (int, name) -> node == name || acc) False (labNodes graph)

-- | returns true iff the given edge contains nodes not in the graph
edgeInGraph :: (T.Text, T.Text, ()) -> Gr T.Text () -> Bool
edgeInGraph (parent, child, _) g = nodeInGraph parent g && nodeInGraph child g

-- | returns a list of all edges that contain nodes not in the graph
edgesInGraph :: [(T.Text, T.Text, ())] -> Gr T.Text () -> [(T.Text, T.Text, ())]
edgesInGraph edges graph = foldl (\acc edge ->  if edgeInGraph edge graph
                                                then edge:acc
                                                else acc ) [] edges

-- | removes all edges in second list from first list
removeEdges :: [(T.Text, T.Text, ())]  -> [(T.Text, T.Text, ())] -> [(T.Text, T.Text, ())]
removeEdges lst rm = foldl (\lst el -> delete el lst) lst rm

-- | wrapper for insMapnode that returns a tuple instead of a triple
-- if the given edge contains nodes not in the graph, the edge is not added
insMapNode' ::  T.Text -> (Gr T.Text (), NodeMap T.Text) -> (Gr T.Text (), NodeMap T.Text)
insMapNode' name (g, m) = let (graph, nodeMap, _) = insMapNode m name g
                          in (graph, nodeMap)

-- | wrapper for insMapNodes that returns a tuple instead of a triple
-- if a given edge contains a node not in the graph, the edge is not added
insMapNodes' :: [T.Text] -> (Gr T.Text (), NodeMap T.Text) -> (Gr T.Text (), NodeMap T.Text)
insMapNodes' names (g, m)  =  let (graph, nodeMap, _) = insMapNodes m names g
                              in (graph, nodeMap)

-- | inserts all valid edges into the graph
insMapEdges' :: [(T.Text, T.Text, ())] -> (Gr T.Text (), NodeMap T.Text) -> (Gr T.Text (), NodeMap T.Text)
insMapEdges' edges (g, m) =
    let safeEdges =  removeEdges (edgesInGraph edges g) edges
    in  (insMapEdges m safeEdges g, m)

-- | inserts the edge, if valid, into the graph
insMapEdge' :: (T.Text, T.Text, ()) -> (Gr T.Text (), NodeMap T.Text) -> (Gr T.Text (), NodeMap T.Text)
insMapEdge' edge (g, m) =
    if edgeInGraph edge g
    then (insMapEdge m edge g, m)
    else (g, m)

-- | parses the prereqs field of the course. atomic prereqs are converted into edges,
-- | and/or prereqs are converted into Contexts
parseCoursesEdges :: [Course] -> GraphPart ->  GraphPart
parseCoursesEdges courses gpart = foldl parseCourseEdge gpart courses

-- | inserts all prerequisites for a given course into the graph
parseCourseEdge :: GraphPart -> Course -> GraphPart
parseCourseEdge gpart course = do
    if (isNothing (prereqs course))
    then gpart
    else let prereqs' = map (T.splitOn " ") (T.splitOn "," (fromJust (prereqs course)))
         in foldl (insertPrereq (name course)) gpart prereqs'

-- | inserts a list of prereqs for a given course into the graph.
insertPrereq ::  T.Text -> GraphPart -> [T.Text] -> GraphPart
insertPrereq name gpart prereq =
    foldl (\g p -> insMapEdge' (p, name, ()) g) gpart prereq

-- | inserts a list of prereqs, including dummy nodes for grouped prerequisites.
-- Currently uses an unsafe method to develop dummy node names.
-- tends to overcomplicate the graph so is currently not used.
insertPrereqDummy :: T.Text -> GraphPart -> [T.Text] -> GraphPart
insertPrereqDummy name gpart prereq =
    if (length prereq == 1)
    then insMapEdge' (head prereq, name, ()) gpart
    else  let nodename = T.concat [name, randomStr]
          -- REMEMBER: these operations are evalated in the reverse order of presentation!
          in  insMapEdge'  (nodename, name, ()) $
              (\g -> foldl (\g p -> insMapEdge' (p, nodename, ()) g) g prereq) $
              insMapNode' nodename gpart
    where randomStr = T.pack $ take 3 $ randomRs ('a','z') $ unsafePerformIO newStdGen

-- | converts a list of Course Records into a Graph
coursesToGraph :: [Course] -> IO (Gr T.Text ())
coursesToGraph courses =
    let noEmpty = filter (\c -> "" /= (name c)) courses
    in return $ fst $ parseCoursesEdges noEmpty $ (mkMapGraph (map name noEmpty) [])

-- | takes a list of course codes, extracts them from Database, converts them
-- into a graph.
toGraph' :: [T.Text] -> IO (Gr T.Text ())
toGraph' courses =
    sequence (map returnCourse courses) >>=
    coursesToGraph

toGraph :: [Course] -> IO (Gr T.Text ())
toGraph courses = coursesToGraph courses

labNodes' :: Gr T.Text () -> IO [LNode T.Text]
labNodes' graph = return (labNodes graph)

main :: IO ()
main =
    toGraph' ["CSC104H1", "CSC108H1", "CSC120H1", "CSC148H1", "CSC165H1",
              "CSC200Y1", "CSC207H1", "CSC209H1", "CSC236H1", "CSC240H1",
              "CSC258H1", "CSC263H1", "CSC265H1", "CSC300H1", "CSC302H1",
              "CSC309H1", "CSC310H1", "CSC318H1", "CSC320H1", "CSC321H1",
              "CSC324H1", "CSC336H1", "CSC343H1", "CSC358H1", "CSC369H1",
              "CSC372H1", "CSC373H1", "CSC384H1", "ECE385H1", "CSC401H1",
              "CSC404H1", "CSC410H1", "CSC411H1", "CSC412H1", "CSC418H1",
              "CSC420H1", "CSC428H1", "CSC436H1", "CSC438H1", "CSC443H1",
              "CSC446H1", "CSC448H1", "CSC454H1", "CSC456H1", "CSC458H1",
              "CSC463H1", "CSC465H1", "CSC469H1", "CSC485H1", "CSC486H1",
              "CSC488H1", "ECE489H1", "CSC490H1",
              "MAT135H1", "MAT136H1", "MAT137Y1", "MAT157Y1", "MAT221H1",
              "MAT223H1", "MAT240H1", "MAT235Y1", "MAT237Y1", "MAT257Y1",
              "STA247H1", "STA255H1", "STA257H1"] >>=
    labNodes' >>=
    print

compsci :: IO (Gr T.Text ())
compsci = toGraph' ["CSC104H1", "CSC108H1", "CSC120H1", "CSC148H1", "CSC165H1",
                    "CSC200Y1", "CSC207H1", "CSC209H1", "CSC236H1", "CSC240H1",
                    "CSC258H1", "CSC263H1", "CSC265H1", "CSC300H1", "CSC302H1",
                    "CSC309H1", "CSC310H1", "CSC318H1", "CSC320H1", "CSC321H1",
                    "CSC324H1", "CSC336H1", "CSC343H1", "CSC358H1", "CSC369H1",
                    "CSC372H1", "CSC373H1", "CSC384H1", "ECE385H1", "CSC401H1",
                    "CSC404H1", "CSC410H1", "CSC411H1", "CSC412H1", "CSC418H1",
                    "CSC420H1", "CSC428H1", "CSC436H1", "CSC438H1", "CSC443H1",
                    "CSC446H1", "CSC448H1", "CSC454H1", "CSC456H1", "CSC458H1",
                    "CSC463H1", "CSC465H1", "CSC469H1", "CSC485H1", "CSC486H1",
                    "CSC488H1", "ECE489H1", "CSC490H1"]
