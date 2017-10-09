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
    ("MAT237H1", J "MAT137H1"),
    ("MAT133H1", NONE),
    ("CSC148H1", AND [J "CSC108H1", J "CSC104H1"]),
    ("CSC265H1", OR [J "CSC240H1", J "CSC236H1"]),
    ("CSC2503H1", FROM "Two of" (J "CSC411H1,CSC412H1,CSC320H1,CSC420H1")),
    ("MAT337H1", GRADE "90%" (J "MAT237H1"))
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
reqToStmts (stmtslst, counter) (name, NONE) = 
    let stmtslst0 = stmtslst ++ [makeNode name counter 0]
        counter0 =  counter + 1
    in  (stmtslst0, counter0)

reqToStmts (_, counter) (name, J string1) = let (stmtslst0, _) = foldUpReqLst [(name, NONE), (str1, NONE)] counter
                                                   in ([makeEdge (str1, counter + 1) (name, counter)] ++ stmtslst0, counter + 2)
  where str1 = pack string1

reqToStmts (stmtslst, counter) (name, AND reqs1) = ([makeNode "and" (counter_sub + 1000) 1, 
                                                    makeEdge ("and", (counter_sub + 1000)) (name, counter)] ++ 
                                                    statements_sub, counter_sub + 1)
  where createSubStmts = foldl(\acc x -> createSingleSubStmt "and" (counter_sub + 1000) acc x)
        (statements_sub, counter_sub) = createSubStmts (reqToStmts (stmtslst, counter) (name, NONE)) (decompJString reqs1)

reqToStmts (stmtslst, counter) (name, OR reqs1) = ([makeNode "or" (counter_sub + 1000) 1, 
                                                    makeEdge ("or", (counter_sub + 1000)) (name, counter)] ++ 
                                                    statements_sub, counter_sub + 1)
  where createSubStmts = foldl(\acc x -> createSingleSubStmt "or" (counter_sub + 1000) acc x)
        (statements_sub, counter_sub) = createSubStmts (reqToStmts (stmtslst, counter) (name, NONE)) (decompJString reqs1)

reqToStmts (stmtslst, counter) (name, FROM string1 (J string2)) = ([makeNode (pack string1) (counter_sub + 1000) 1, 
                                                                    makeEdge ((pack string1), (counter_sub + 1000)) (name, counter)] ++ 
                                                                    statements_sub, counter_sub + 1)
  where createSubStmts = foldl(\acc x -> createSingleSubStmt (pack string1) (counter_sub + 1000) acc x)
        (statements_sub, counter_sub) = createSubStmts (reqToStmts (stmtslst, counter) (name, NONE)) [((pack string2), NONE)]

reqToStmts (stmtslst, counter) (name, GRADE string1 (J string2)) = ([makeNode (pack string1) (counter_sub + 1000) 1, 
                                                                    makeEdge ((pack string1), (counter_sub + 1000)) (name, counter)] ++ 
                                                                    statements_sub, counter_sub + 1)
  where createSubStmts = foldl(\acc x -> createSingleSubStmt (pack string1) (counter_sub + 1000) acc x)
        (statements_sub, counter_sub) = createSubStmts (reqToStmts (stmtslst, counter) (name, NONE)) [((pack string2), NONE)]

reqToStmts (_, counter) (name, RAW string1) = ([makeNode (pack string1) counter 0, 
                                                       makeEdge ((pack string1), counter) (name, counter)], counter + 1)



foldUpReqLst :: [(Text, Req)] -> Int -> StmtsWithCounter
foldUpReqLst reqlst count = foldl reqToStmts ([], count) reqlst

createSubnodeCorrespondingEdge :: Text -> StmtsWithCounter -> (Text, Req) -> StmtsWithCounter
createSubnodeCorrespondingEdge parentnode (stmtslst, counter) (name, NONE) = 
    let stmtslst1 = stmtslst ++ [makeEdge (name, counter) (parentnode, -1)]
    in reqToStmts (stmtslst1, counter) (name, NONE)

createSingleSubStmt :: Text -> Int -> (StmtsWithCounter -> (Text, Req) -> StmtsWithCounter)
createSingleSubStmt text1 counter = createSubnodeCorrespondingEdge (mappendTextWithCounter text1 counter)

makeNode :: Text -> Int -> Int -> DotStatement Text
makeNode text1 counter 0 = DN $ DotNode (mappendTextWithCounter text1 counter) []
makeNode text1 counter 1 = DN $ DotNode (mappendTextWithCounter text1 counter) [A.shape A.Square, AC.Width 1]

makeEdge :: (Text, Int) -> (Text, Int) -> DotStatement Text
makeEdge (name1, -1) (name2, -1) = DE $ DotEdge name1 name2 []
makeEdge (name1, counter1) (name2, -1) = DE $ DotEdge (mappendTextWithCounter name1 counter1) name2 []
makeEdge (name1, counter1) (name2, counter2) = DE $ DotEdge (mappendTextWithCounter name1 counter1) 
                                               (mappendTextWithCounter name2 counter2) []

mappendTextWithCounter :: Text -> Int -> Text
mappendTextWithCounter text1 counter = text1 `mappend` "_counter_" `mappend` (pack (show (counter))) 

-- Now this only wotks for Req lists of J String. Failed if using [Req] as input and pack x in foldl.
decompJString :: [Req] -> [(Text, Req)]
decompJString [] = []
decompJString ((J x):xs) = (pack x, NONE):(decompJString xs)

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
nodeAttrs = NodeAttrs [A.shape A.Circle, AC.Width 4, A.style A.filled]

-- Using default setting for the edges connecting the nodes.
edgeAttrs :: GlobalAttributes
edgeAttrs = EdgeAttrs []
