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


-- Serves as a sort of "interface" for the whole part "dynamic graph"
sampleGraph :: DotGraph Text
sampleGraph = reqsToGraph [
    ("CSC148", J "CSC108")
    ]


-- ** Main algorithm for converting requirements into a graph

-- (not so sure) reqToStmts are meant to convert a single requirement and reqsToGraph use concatMap to 
-- use reqToStmts to converts a list of requirements all at once and concatenate the results into a 
-- single list of DotGraph objects.
reqsToGraph :: [(Text, Req)] -> DotGraph Text
reqsToGraph reqs =
    let stmts = stmtslst (foldUpReqLst reqs 0)
    in
        buildGraph stmts

-- convert the original requirement data into dot statements that can be used by buildGraph to create the
-- corresponding DotGraph objects. ([] is the [] after name the optional parameters for DotNode?)
reqToStmts :: (Text, Req) -> StmtsWithCounter -> StmtsWithCounter


reqToStmts (name, NONE) stmtcounter = StmtsWithCounter{stmtslst = connectLst (stmtslst stmtcounter) [DN $ DotNode (name `mappend` (pack (show (counter stmtcounter)))) []]
                                      , counter = (counter stmtcounter) + 1}




reqToStmts (name, J string1) stmtcounter = StmtsWithCounter{stmtslst = connectLst [DE $ DotEdge str1 name []] (stmtslst (foldUpReqLst [(name, NONE), (str1, NONE)] 0))
                                                           , counter = (counter stmtcounter) + 3}
  where str1 = pack string1

--reqToStmts (name, AND reqs1) stmtcounter = connectLst [DN $ DotNode "and" [], DE $ DotEdge "and" name []] (connectLst andedges nodestatements)
--  where andedges = createAndEdge (dragNodeIdInReq reqs1)
--       nodestatements = concatMap reqToStmts ((name, NONE):(decompJString reqs1))

--reqToStmts (name, OR reqs1) stmtcounter = connectLst [DN $ DotNode "or" [], DE $ DotEdge "or" name []] (connectLst oredges nodestatements)
--  where oredges = createOrEdge (dragNodeIdInReq reqs1)
--        nodestatements = concatMap reqToStmts ((name, NONE):(decompJString reqs1))

--reqToStmts (name, FROM string1 (J string2)) stmtcounter = connectLst fromedges (concatMap reqToStmts [(name, NONE), (pack_string1, NONE), (pack_string2, NONE)])
--  where pack_string1 = pack string1
--        pack_string2 = pack string2
--        fromedges = [DE $ DotEdge pack_string2 pack_string1 [], DE $ DotEdge pack_string1 name []]

--reqToStmts (name, GRADE string1 (J string2)) stmtcounter = connectLst gradeedges (concatMap reqToStmts [(name, NONE), (pack string1, NONE), (pack string2, NONE)])
--  where pack_string1 = pack string1
--        pack_string2 = pack string2
--        gradeedges = [DE $ DotEdge pack_string2 pack_string1 [], DE $ DotEdge pack_string1 name []]

--reqToStmts (name, RAW string1) stmtcounter = connectLst rawedges (concatMap reqToStmts [(name, NONE), (pack_string1, NONE)])
--  where pack_string1 = pack string1
--        rawedges = [DE $ DotEdge pack_string1 name []]

--reqToStmts (_, _) stmtcounter = []

data StmtsWithCounter = StmtsWithCounter{ stmtslst :: [DotStatement Text]
                                        , counter   :: Int }

foldUpReqLst :: [(Text, Req)] -> Int -> StmtsWithCounter
foldUpReqLst reqlst count = foldl(\acc x -> reqToStmts x acc) StmtsWithCounter{stmtslst = [], counter = count} reqlst




decompJString :: [Req] -> [(Text, Req)]
decompJString [] = []
decompJString ((J x):xs) = (pack x, NONE):(decompJString xs)

createAndEdge :: [Text] -> [DotStatement Text]
createAndEdge [] = []
createAndEdge (x:xs) = connectLst [DE $ DotEdge x "and" []] (createAndEdge xs)

createOrEdge :: [Text] -> [DotStatement Text]
createOrEdge [] = []
createOrEdge (x:xs) = connectLst [DE $ DotEdge x "or" []] (createOrEdge xs)

dragNodeIdInReq :: [Req] -> [Text]
dragNodeIdInReq [] = []
dragNodeIdInReq ((J text1):xs) = (pack text1):(dragNodeIdInReq xs)

connectLst :: [a] -> [a] -> [a]
connectLst xs [] = xs
connectLst [] ys = ys
connectLst (x:xs) ys = connectLst xs (x:ys)


-- ** Graphviz configuration

-- with the dot statements converted from original requirement data as input, create the corresponding DotGraph
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

-- means the layout of the full graph is from left to right.
graphAttrs :: GlobalAttributes
graphAttrs = GraphAttrs [AC.RankDir AC.FromLeft]

-- means the shape of each node in the graph is circle with width 1, and is filled.
nodeAttrs :: GlobalAttributes
nodeAttrs = NodeAttrs [A.shape A.Circle, AC.Width 1, A.style A.filled]

-- using default setting for the edges connecting the nodes.
edgeAttrs :: GlobalAttributes
edgeAttrs = EdgeAttrs []

