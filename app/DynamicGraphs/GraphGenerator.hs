{-# LANGUAGE OverloadedStrings #-}

module DynamicGraphs.GraphGenerator
  (sampleGraph)
  where

import Data.GraphViz.Attributes as A
import Data.GraphViz.Attributes.Complete as AC
import Data.GraphViz.Types.Generalised (
  DotGraph(..),
  DotNode(..),
  DotStatement(..),
  GlobalAttributes(..))
import Database.Requirement (Req(..))
import Data.Sequence as Seq
import Data.Text.Lazy (Text)


sampleGraph :: DotGraph Text
sampleGraph = reqsToGraph [
    ("CSC108", NONE)
    ]


-- ** Main algorithm for converting requirements into a graph
reqsToGraph :: [(Text, Req)] -> DotGraph Text
reqsToGraph reqs =
    let stmts = concatMap reqToStmts reqs
    in
        buildGraph stmts

reqToStmts :: (Text, Req) -> [DotStatement Text]
reqToStmts (name, NONE) = [DN $ DotNode name []]
reqToStmts (_, _) = []


-- ** Graphviz configuration
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

graphAttrs :: GlobalAttributes
graphAttrs = GraphAttrs [AC.RankDir AC.FromLeft]

nodeAttrs :: GlobalAttributes
nodeAttrs = NodeAttrs [A.shape A.Circle, AC.Width 1, A.style A.filled]

edgeAttrs :: GlobalAttributes
edgeAttrs = EdgeAttrs []
