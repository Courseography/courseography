{-# LANGUAGE OverloadedStrings #-}

module DynamicGraphs.GraphGenerator
  ( sampleGraph
  , nodeToGraph
  )
  where

import Data.GraphViz.Attributes as A
import Data.GraphViz.Attributes.Complete as AC
import Data.GraphViz.Types.Generalised (
  DotEdge(..),
  DotGraph(..),
  DotNode(..),
  DotStatement(..),
  GlobalAttributes(..)
  )
import Database.Requirement (Req(..))
import DynamicGraphs.Node (Node(..))
import Data.Sequence as Seq
import Data.Text.Lazy (Text, pack)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Monad (mapM)


-- Serves as a sort of "interface" for the whole part "dynamic graph"
nodeToGraph :: Node -> DotGraph Text
nodeToGraph node = fst $ State.runState (buildGraphFromNode node) 0

sampleGraph :: DotGraph Text
sampleGraph = fst $ State.runState (reqsToGraph [
    ("MAT237H1", J "MAT137H1" ""),
    ("MAT133H1", NONE),
    ("CSC148H1", AND [J "CSC108H1" "", J "CSC104H1" ""]),
    ("CSC265H1", AND [J "CSC148H1" "", J "CSC236H1" ""])
    ])
    0

--
-- ** Main algorithm for converting requirements into a graph

-- The reqToStmts are meant to convert a single requirement and reqsToGraph use concatMap to
-- use reqToStmts to converts a list of requirements all at once and concatenate the results into a
-- single list of DotGraph objects.
reqsToGraph :: [(Text, Req)] -> State Integer (DotGraph Text)
reqsToGraph reqs = do
    allStmts <- mapM reqToStmts reqs
    return $ buildGraph $ concat allStmts

buildGraphFromNode :: Node -> State Integer (DotGraph Text)
buildGraphFromNode node = nodeToStmts node >>= (return . buildGraph)

nodeToStmts :: Node -> State Integer [DotStatement Text]
nodeToStmts (Leaf name) = do
    node <- makeNode $ pack name
    return [DN node]
nodeToStmts (Parent name child) = do
    node <- makeNode $ pack name
    children <- nodeToStmts child
    edge <- makeEdge (nodeID $ headNode children) (nodeID node)
    return $ [DN node, DE edge] ++ children
nodeToStmts (Conj children) = nodeToBoolStmts "and" children
nodeToStmts (Disj children) = nodeToBoolStmts "or" children

nodeToBoolStmts :: Text -> [Node] -> State Integer [DotStatement Text]
nodeToBoolStmts name children = do
    node <- makeBool name
    prereqStmts <- mapM nodeToStmts children
    -- Create edges from each eldest prereq to this boolean node.
    let childNodes = map headNode prereqStmts
        pointToThis child = makeEdge (nodeID child) (nodeID node)
    edges <- mapM pointToThis childNodes
    return $ DN node : map DE edges ++ concat prereqStmts

headNode :: [DotStatement Text] -> DotNode Text
headNode children =
    let (DN node) = head children
    in node

-- Convert the original requirement data into dot statements that can be used by buildGraph to create the
-- corresponding DotGraph objects.
reqToStmts :: (Text, Req) -> State Integer [DotStatement Text]
reqToStmts (name, req) = do
    node <- makeNode name
    stmts <- reqToStmts' (nodeID node) req
    return $ (DN node):stmts

reqToStmts' :: Text -> Req -> State Integer [DotStatement Text]
-- No prerequisites.
reqToStmts' _ NONE = return []
-- A single course prerequisite.
reqToStmts' parentID (J name2 _) = do
    prereq <- makeNode (pack name2)
    edge <- makeEdge (nodeID prereq) parentID
    return [DN prereq, DE edge]
-- Two or more required prerequisites.
reqToStmts' parentID (AND reqs) = do
    andNode <- makeBool "and"
    edge <- makeEdge (nodeID andNode) parentID
    prereqStmts <- mapM (reqToStmts' (nodeID andNode)) reqs
    return $ [DN andNode, DE edge] ++ concat prereqStmts
-- A choice from two or more prerequisites.
reqToStmts' parentID (OR reqs) = do
    orNode <- makeBool "or"
    edge <- makeEdge (nodeID orNode) parentID
    prereqStmts <- mapM (reqToStmts' (nodeID orNode)) reqs
    return $ [DN orNode, DE edge] ++ concat prereqStmts
-- A prerequisite with a grade requirement.
reqToStmts' parentID (GRADE description req) = do
    gradeNode <- makeNode (pack description)
    edge <- makeEdge (nodeID gradeNode) parentID
    prereqStmts <- reqToStmts' (nodeID gradeNode) req
    return $ [DN gradeNode, DE edge] ++ prereqStmts

-- A raw string description of a prerequisite.
reqToStmts' parentID (RAW rawText) = do
    prereq <- makeNode (pack rawText)
    edge <- makeEdge (nodeID prereq) parentID
    return [DN prereq, DE edge]

-- TODO: Complete this one.
reqToStmts' _ (FCES _ _) = return []


makeNode :: Text -> State Integer (DotNode Text)
makeNode name = do
    i <- State.get
    _ <- State.put (i + 1)
    return $ DotNode (mappendTextWithCounter name i) [AC.Label $ toLabelValue name]

makeBool :: Text -> State Integer (DotNode Text)
makeBool text1 = do
    i <- State.get
    _ <- State.put (i + 1)
    return $ DotNode (mappendTextWithCounter text1 i) (AC.Label (toLabelValue text1) : ellipseAttrs)


makeEdge :: Text -> Text -> State Integer (DotEdge Text)
makeEdge id1 id2 = return $ DotEdge id1 id2 []

mappendTextWithCounter :: Text -> Integer -> Text
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

ellipseAttrs :: A.Attributes
ellipseAttrs = [
    A.shape A.Ellipse,
    AC.Width 1,
    AC.Height 0.5,
    A.fillColor White
    ]
-- Using default setting for the edges connecting the nodes.
edgeAttrs :: GlobalAttributes
edgeAttrs = EdgeAttrs []
