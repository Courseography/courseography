{-# LANGUAGE OverloadedStrings #-}

module DynamicGraphs.GraphGenerator
  ( sampleGraph
  , coursesToPrereqGraph
  , coursesToPrereqGraphExcluding
  , graphProfileHash
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
import DynamicGraphs.CourseFinder (lookupCourses)
import qualified Data.Map.Strict as Map
import Database.Requirement (Req(..))
import Data.Sequence as Seq
import Data.Hash.MD5 (Str(Str), md5s)
import Data.Text.Lazy (Text, pack, unpack, isInfixOf)
import Data.Containers.ListUtils (nubOrd)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Monad (mapM, liftM)
import DynamicGraphs.GraphOptions (GraphOptions(..), getDefaultGraphOptions)

-- | Generates a DotGraph dependency graph including all the given courses and their recursive dependecies
coursesToPrereqGraph :: [String] -- ^ courses to generate
                        -> IO (DotGraph Text)
coursesToPrereqGraph rootCourses = coursesToPrereqGraphExcluding (map pack rootCourses) getDefaultGraphOptions

-- | Takes a list of taken courses, along with a list of courses we wish to generate
-- a dependency graph for. The generated graph will neither include any of the taken courses,
-- nor the dependencies of taken courses (unless they are depended on by other courses)
coursesToPrereqGraphExcluding :: [Text] -> GraphOptions -> IO (DotGraph Text)
coursesToPrereqGraphExcluding rootCourses options = do
    reqs <- lookupCourses (map unpack (taken options)) rootCourses
    let reqs' = Map.toList reqs
    return $ fst $ State.runState (reqsToGraph reqs') initialState
    where
        initialState = GeneratorState 0 Map.empty

sampleGraph :: DotGraph Text
sampleGraph = fst $ State.runState (reqsToGraph [
    ("MAT237H1", J "MAT137H1" ""),
    ("MAT133H1", NONE),
    ("CSC148H1", AND [J "CSC108H1" "", J "CSC104H1" ""]),
    ("CSC265H1", AND [J "CSC148H1" "", J "CSC236H1" ""])
    ])
    (GeneratorState 0 Map.empty)


-- ** Main algorithm for converting requirements into a DotGraph

-- | Convert a list of coursenames and requirements to a DotGraph object for
--  drawing using Dot. Also prunes any repeated edges that arise from
--  multiple Reqs using the same GRADE requirement
reqsToGraph :: [(Text, Req)] -> State GeneratorState (DotGraph Text)
reqsToGraph reqs = do
    allStmts <- liftM concatUnique $ mapM reqToStmts reqs
    return $ buildGraph allStmts
    where
        concatUnique = nubOrd . concat

data GeneratorState = GeneratorState Integer (Map.Map Text (DotNode Text))

-- | Convert the original requirement data into dot statements that can be used by buildGraph to create the
-- corresponding DotGraph objects.
reqToStmts :: (Text, Req) -> State GeneratorState [DotStatement Text]
reqToStmts (name, req) = do
    node <- makeNode name
    stmts <- reqToStmts' (nodeID node) req
    return $ (DN node):stmts

reqToStmts' :: Text -> Req -> State GeneratorState [DotStatement Text]
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
reqToStmts' parentID (RAW rawText) = 
    if "High school" `isInfixOf` pack rawText || rawText == ""
        then return []
        else do
    prereq <- makeNode (pack rawText)
    edge <- makeEdge (nodeID prereq) parentID
    return [DN prereq, DE edge]

--A prerequisite concerning a given number of earned credits
reqToStmts' parentID (FCES creds req) = do
    fceNode <- makeNode (pack $ "at least " ++ creds ++ " FCEs")
    edge <- makeEdge (nodeID fceNode) parentID
    prereqStmts <- reqToStmts' (nodeID fceNode) req 
    return $  [DN fceNode, DE edge] ++ prereqStmts

makeNode :: Text -> State GeneratorState (DotNode Text)
makeNode name = do
    GeneratorState i nodesMap <- State.get
    case Map.lookup name nodesMap of
        Nothing -> do
            let nodeId = mappendTextWithCounter name i
                node = DotNode nodeId
                               [AC.Label $ toLabelValue name, ID nodeId]
                nodesMap' = Map.insert name node nodesMap
            State.put (GeneratorState (i + 1) nodesMap')
            return node
        Just node -> return node

makeBool :: Text -> State GeneratorState (DotNode Text)
makeBool text1 = do
    GeneratorState i nodesMap <- State.get
    State.put (GeneratorState (i + 1) nodesMap)
    let nodeId = mappendTextWithCounter text1 i
    return $ DotNode nodeId
                     ([AC.Label (toLabelValue text1), ID nodeId] ++ ellipseAttrs)


makeEdge :: Text -> Text -> State GeneratorState (DotEdge Text)
makeEdge id1 id2 = return $ DotEdge id1 id2 [ID (id1 `mappend` "|" `mappend` id2)]

mappendTextWithCounter :: Text -> Integer -> Text
mappendTextWithCounter text1 counter = text1 `mappend` "_counter_" `mappend` (pack (show counter))

-- ** Graphviz configuration

-- | With the dot statements converted from original requirement data as input, create the corresponding DotGraph
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

graphProfileHash :: String
graphProfileHash = md5s . Str . show $ (buildGraph [], ellipseAttrs)

-- | Means the layout of the full graph is from left to right.
graphAttrs :: GlobalAttributes
graphAttrs = GraphAttrs 
    [ AC.RankDir AC.FromTop
    , AC.Splines AC.Ortho
    , AC.Concentrate False
    ]

nodeAttrs :: GlobalAttributes
nodeAttrs = NodeAttrs 
    [ A.shape A.BoxShape
    , AC.FixedSize GrowAsNeeded
    , A.style A.filled
    ]

ellipseAttrs :: A.Attributes
ellipseAttrs = 
    [ A.shape A.Ellipse
    , AC.Width 0.20     -- min 0.01
    , AC.Height 0.15    -- min 0.01
    , AC.FixedSize SetNodeSize
    , A.fillColor White
    , AC.FontSize 6.0  -- min 1.0
    ]

edgeAttrs :: GlobalAttributes
edgeAttrs = EdgeAttrs [
    ArrowHead (AType [(ArrMod FilledArrow BothSides, Normal)])
    ]
