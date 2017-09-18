{-# LANGUAGE OverloadedStrings #-}

module DynamicGraphs.GraphGenerator
  (sampleGraph)
  where

import Data.GraphViz.Attributes as A
import Data.GraphViz.Attributes.Complete as AC
import Data.GraphViz.Types.Generalised (
  DotEdge(..),
  DotGraph(..),
  DotNode(..),
  DotStatement(..),
  GlobalAttributes(..))
import Database.Requirement (Req(..))
import Data.Sequence as Seq
import Data.Text.Lazy (Text, pack)

type StmtsWithCounter = ([DotStatement Text], Int)

-- Serves as a sort of "interface" for the whole part "dynamic graph"
sampleGraph :: DotGraph Text
sampleGraph = reqsToGraph [
    ("CSC148", J "CSC108")
    ]

-- ** Main algorithm for converting requirements into a graph

-- The reqToStmts are meant to convert a single requirement and reqsToGraph use concatMap to 
-- use reqToStmts to converts a list of requirements all at once and concatenate the results into a 
-- single list of DotGraph objects.
reqsToGraph :: [(Text, Req)] -> DotGraph Text
reqsToGraph reqs =
    let (stmts, _) = foldUpReqLst reqs 0
    in
        buildGraph stmts

-- Convert the original requirement data into dot statements that can be used by buildGraph to create the
-- corresponding DotGraph objects. ([] is the [] after name the optional parameters for DotNode?)
reqToStmts :: StmtsWithCounter -> (Text, Req) -> StmtsWithCounter
reqToStmts (stmtslst, counter) (name, NONE) = let stmtslst0 = let stmtslst1 = stmtslst 
                                                                  counter1 = counter
                                                              in  stmtslst1 ++ [DN $ DotNode (name `mappend` (pack (show (counter1)))) []]
                                                  counter0 =  let counter2 = counter
                                                              in  (counter2) + 1
                                              in  (stmtslst0, counter0)
reqToStmts (stmtslst, counter) (name, J string1) = let (stmtslst0, _) = foldUpReqLst [(name, NONE), (str1, NONE)] 0
                                                   in ([DE $ DotEdge str1 name []] ++ stmtslst0, counter + 3)
  where str1 = pack string1

foldUpReqLst :: [(Text, Req)] -> Int -> StmtsWithCounter
foldUpReqLst reqlst count = foldl(\acc x -> reqToStmts acc x) ([], count) reqlst

-- Now this only wotks for Req lists of J String. Failed if using [Req] as input and pack x in foldl.
decompJString :: [String] -> [(Text, Req)]
decompJString [] = []
decompJString xs = foldl(\acc x -> (pack x, NONE):acc) [] xs

createAndEdge :: [Text] -> [DotStatement Text]
createAndEdge [] = []
createAndEdge xs = foldl(\acc x -> [DE $ DotEdge x "and" []] ++ acc) [] xs

createOrEdge :: [Text] -> [DotStatement Text]
createOrEdge [] = []
createOrEdge xs = foldl(\acc x -> [DE $ DotEdge x "or" []] ++ acc) [] xs

dragNodeIdInReq :: [Req] -> [Text]
dragNodeIdInReq [] = []
dragNodeIdInReq ((J text1):xs) = (pack text1):(dragNodeIdInReq xs)

-- ** Graphviz configuration

-- With the dot statements converted from original requirement data as input, create the corresponding DotGraph
-- object with predefined hyperparameters (here, the hyperparameters defines that 1.graph can have multi-edges
-- 2.graph edges have directions 3.graphID not defined(not so clear) 4.the graph layout, node shape, edge shape 
-- are defined by the attributes as below)
buildGraph :: [DotStatement Text] -> DotGraph Text
buildGraph statements = DotGraph {
    strictGraph = False,
    directedGraph = True,
    graphID = Nothing,
    graphStatements = Seq.fromList $ [
        GA graphAttrs,
        GA nodeAttrs,
        GA edgeAttrs
        ] ++ statements
    }

-- Means the layout of the full graph is from left to right.
graphAttrs :: GlobalAttributes
graphAttrs = GraphAttrs [AC.RankDir AC.FromLeft]

-- Means the shape of each node in the graph is circle with width 1, and is filled.
nodeAttrs :: GlobalAttributes
nodeAttrs = NodeAttrs [A.shape A.Circle, AC.Width 1, A.style A.filled]

-- Using default setting for the edges connecting the nodes.
edgeAttrs :: GlobalAttributes
edgeAttrs = EdgeAttrs []
