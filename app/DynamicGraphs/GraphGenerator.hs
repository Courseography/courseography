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
-- TODO: update function
    ("MAT237H1", J "MAT137H1" ""),
    ("MAT133H1", NONE),
    ("CSC148H1", AND [J "CSC108H1" "", J "CSC104H1" ""]),
    ("CSC265H1", AND [J "CSC148H1" "", J "CSC236H1" ""])
    ]

--
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
reqToStmts (stmtslst, counter) (name, NONE) =
    let stmtslst0 = stmtslst ++ [makeNode name counter 0]
        counter0 =  counter + 1
    in  (stmtslst0, counter0)

-- TODO: update function
reqToStmts (stmtslst, counter) (name, J string1 string2) = (stmtslst1 ++ connectRootsToName roots1 uppernode0, counter1)

  where stmtslst0 = [makeNode name counter 0] ++ stmtslst
        uppernode0 = mappendTextWithCounter name counter
        ((stmtslst1, counter1), roots1) = reqToStmtsHelper ((stmtslst0, counter + 1), []) (J string1 string2)

reqToStmts (stmtslst, counter) (name, AND reqs1) = (stmtslst1 ++ connectRootsToName roots1 uppernode0, counter1)

  where stmtslst0 = [makeNode name counter 0] ++ stmtslst
        uppernode0 = mappendTextWithCounter name counter
        ((stmtslst1, counter1), roots1) = reqToStmtsHelper ((stmtslst0, counter + 1), []) (AND reqs1)

reqToStmts (stmtslst, counter) (name, OR reqs1) = (stmtslst1 ++ connectRootsToName roots1 uppernode0, counter1)

  where stmtslst0 = [makeNode name counter 0] ++ stmtslst
        uppernode0 = mappendTextWithCounter name counter
        ((stmtslst1, counter1), roots1) = reqToStmtsHelper ((stmtslst0, counter + 1), []) (OR reqs1)

reqToStmts (stmtslst, counter) (name, FCES string1 reqs1) = (stmtslst1 ++ connectRootsToName roots1 uppernode0, counter1)

  where uppernode0 = mappendTextWithCounter (pack string1) (counter + 1)
        namewithcounter = mappendTextWithCounter name counter
        stmtslst0 = [makeNode name counter 0] ++ [makeNode (pack string1) (counter + 1) 1] ++
                    [makeEdge (uppernode0, -1) (namewithcounter, -1)] ++ stmtslst
        ((stmtslst1, counter1), roots1) = reqToStmtsHelper ((stmtslst0, counter + 2), []) reqs1

-- TODO: update function
reqToStmts (stmtslst, counter) (name, GRADE string1 (J string2 string3)) = (stmtslst1 ++ connectRootsToName roots1 uppernode0, counter1)

  where uppernode0 = mappendTextWithCounter (pack string1) (counter + 1)
        namewithcounter = mappendTextWithCounter name counter
        stmtslst0 = [makeNode name counter 0] ++ [makeNode (pack string1) (counter + 1) 1] ++
                    [makeEdge (uppernode0, -1) (namewithcounter, -1)] ++ stmtslst
-- TODO: update function
        ((stmtslst1, counter1), roots1) = reqToStmtsHelper ((stmtslst0, counter + 2), []) (J string2 string3)

reqToStmts (stmtslst, counter) (name, RAW string1) = (stmtslst1 ++ connectRootsToName roots1 uppernode0, counter1)

  where stmtslst0 = [makeNode name counter 0] ++ stmtslst
        uppernode0 = mappendTextWithCounter name counter
        ((stmtslst1, counter1), roots1) = reqToStmtsHelper ((stmtslst0, counter + 1), []) (RAW string1)

reqToStmts _ _ = undefined

reqToStmtsHelper :: (StmtsWithCounter, [Text]) -> Req -> (StmtsWithCounter, [Text])

-- TODO: update function
reqToStmtsHelper ((stmtslst, counter), roots) (J string1 _) = (([makeNode (pack string1) counter 0]
                                                            ++ stmtslst, counter + 1),
                                                            [mappendTextWithCounter (pack string1) counter] ++ roots)

reqToStmtsHelper ((stmtslst, counter), roots) (AND reqs1) = (([makeNode "and" (counter + 1000) 1]
                                                            ++ stmtslst0 ++
                                                            (connectRootsToName roots1 rootwithcounter), counter0),
                                                            [rootwithcounter] ++ roots)

  where rootwithcounter = mappendTextWithCounter "and" (counter + 1000)
        ((stmtslst0, counter0), roots1) = foldl(\acc x -> reqToStmtsHelper acc x) ((stmtslst, counter + 1), []) reqs1

reqToStmtsHelper ((stmtslst, counter), roots) (OR reqs1) = (([makeNode "or" (counter + 1000) 1]
                                                            ++ stmtslst0 ++
                                                            (connectRootsToName roots1 rootwithcounter), counter0),
                                                            [rootwithcounter] ++ roots)

  where rootwithcounter = mappendTextWithCounter "or" (counter + 1000)
        ((stmtslst0, counter0), roots1) = foldl(\acc x -> reqToStmtsHelper acc x) ((stmtslst, counter + 1), []) reqs1

reqToStmtsHelper ((stmtslst, counter), roots) (RAW string1) = (([makeNode (pack string1) counter 0]
                                                            ++ stmtslst, counter + 1),
                                                            [mappendTextWithCounter (pack string1) counter] ++ roots)

reqToStmtsHelper _ _ = undefined

connectRootsToName :: [Text] -> Text -> [DotStatement Text]
connectRootsToName roots name1 = foldl(\acc x -> acc ++ [(makeEdge (x, -1) (name1, -1))]) [] roots

foldUpReqLst :: [(Text, Req)] -> Int -> StmtsWithCounter
foldUpReqLst reqlst count = foldl reqToStmts ([], count) reqlst

makeNode :: Text -> Int -> Int -> DotStatement Text
makeNode text1 counter 0 = DN $ DotNode (mappendTextWithCounter text1 counter) [AC.Label (toLabelValue text1)]
makeNode text1 counter 1 = DN $ DotNode (mappendTextWithCounter text1 counter) [AC.Label (toLabelValue text1), A.shape A.Ellipse, AC.Width 1, AC.Height 0.5, A.fillColor White]
makeNode _ _ _ = undefined

makeEdge :: (Text, Int) -> (Text, Int) -> DotStatement Text
makeEdge (name1, -1) (name2, -1) = DE $ DotEdge name1 name2 []
makeEdge (name1, counter1) (name2, -1) = DE $ DotEdge (mappendTextWithCounter name1 counter1) name2 []
makeEdge (name1, counter1) (name2, counter2) = DE $ DotEdge (mappendTextWithCounter name1 counter1)
                                               (mappendTextWithCounter name2 counter2) []

mappendTextWithCounter :: Text -> Int -> Text
mappendTextWithCounter text1 counter = text1 `mappend` "_counter_" `mappend` (pack (show (counter)))

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
nodeAttrs = NodeAttrs [A.shape A.BoxShape, AC.Width 2, AC.Height 1, A.style A.filled]

-- Using default setting for the edges connecting the nodes.
edgeAttrs :: GlobalAttributes
edgeAttrs = EdgeAttrs []