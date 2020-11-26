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
import Data.Text.Lazy (Text, pack, unpack, isInfixOf, isPrefixOf)
import Data.Containers.ListUtils (nubOrd)
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Monad (mapM, liftM)
import DynamicGraphs.GraphOptions (GraphOptions(..))

-- | Generates a DotGraph dependency graph including all the given courses and their recursive dependecies
coursesToPrereqGraph :: [String] -- ^ courses to generate
                        -> IO (DotGraph Text)
coursesToPrereqGraph rootCourses = coursesToPrereqGraphExcluding (getDefaultGraphOptions (map pack rootCourses))

-- | Takes a list of taken courses, along with a list of courses we wish to generate
-- a dependency graph for. The generated graph will neither include any of the taken courses,
-- nor the dependencies of taken courses (unless they are depended on by other courses)
coursesToPrereqGraphExcluding :: GraphOptions
                              -> IO (DotGraph Text)
coursesToPrereqGraphExcluding options = do
    reqs <- lookupCourses (map unpack (taken options)) (courses options)
    let reqs' = Map.toList reqs
    return $ fst $ State.runState (reqsToGraph options reqs') initialState
    where
        initialState = GeneratorState 0 Map.empty

sampleGraph :: DotGraph Text
sampleGraph = fst $ State.runState (reqsToGraph
    (getDefaultGraphOptions ["MAT237H1", "MAT133H1", "CSC148H1", "CSC265H1"])
    [("MAT237H1", J "MAT137H1" ""),
    ("MAT133H1", NONE),
    ("CSC148H1", AND [J "CSC108H1" "", J "CSC104H1" ""]),
    ("CSC265H1", AND [J "CSC148H1" "", J "CSC236H1" ""])
    ])
    (GeneratorState 0 Map.empty)

getDefaultGraphOptions :: [Text] -> GraphOptions
getDefaultGraphOptions rootCourses =
    GraphOptions rootCourses           -- courses
                []                     -- taken
                []                     -- departments
                0                      -- excludedDepth
                (-1)                   -- maxDepth
                []                     -- courseNumPrefix
                []                     -- distribution
                []                     -- location
                True                   -- includeRaws
                True                   -- includeGrades

-- ** Main algorithm for converting requirements into a DotGraph

-- | Convert a list of coursenames and requirements to a DotGraph object for
--  drawing using Dot. Also prunes any repeated edges that arise from
--  multiple Reqs using the same GRADE requirement
reqsToGraph :: GraphOptions -> [(Text, Req)] -> State GeneratorState (DotGraph Text)
reqsToGraph options reqs = do
    allStmts <- liftM concatUnique $ mapM (reqToStmts options) reqs
    return $ buildGraph allStmts
    where
        concatUnique = nubOrd . concat

data GeneratorState = GeneratorState Integer (Map.Map Text (DotNode Text))

-- | Convert the original requirement data into dot statements that can be used by buildGraph to create the
-- corresponding DotGraph objects.
reqToStmts :: GraphOptions -> (Text, Req) -> State GeneratorState [DotStatement Text]
reqToStmts options (name, req) = do
    if Prelude.null (departments options)|| prefixedByOneOf name (departments options)
        then do 
            node <- makeNode name
            stmts <- reqToStmts' options (nodeID node) req
            return $ (DN node):stmts
        else return []

reqToStmts' :: GraphOptions -> Text -> Req -> State GeneratorState [DotStatement Text]
-- No prerequisites.
reqToStmts' _ _ NONE = return []
-- A single course prerequisite.
reqToStmts' options parentID (J name2 _) = do
    if Prelude.null (departments options) || prefixedByOneOf (pack name2) (departments options)
        then do 
            prereq <- makeNode (pack name2)
            edge <- makeEdge (nodeID prereq) parentID
            return [DN prereq, DE edge]
        else return []        
-- Two or more required prerequisites.
reqToStmts' options parentID (AND reqs) = do
    if includeRaws options || atLeastTwoCourseReqs reqs
        then do
            andNode <- makeBool "and"
            edge <- makeEdge (nodeID andNode) parentID
            prereqStmts <- mapM (reqToStmts' options (nodeID andNode)) reqs
            let mergedStmts = concat prereqStmts
            if Prelude.length mergedStmts > 1
                then return $ [DN andNode, DE edge] ++ concat prereqStmts
                else return []
        else do
            prereqStmts <- mapM (reqToStmts' options parentID) reqs
            return $ concat prereqStmts
-- A choice from two or more prerequisites.
reqToStmts' options parentID (OR reqs) = do
    if includeRaws options || atLeastTwoCourseReqs reqs
        then do
            orNode <- makeBool "or"
            edge <- makeEdge (nodeID orNode) parentID
            prereqStmts <- mapM (reqToStmts' options (nodeID orNode)) reqs
            let mergedStmts = concat prereqStmts
            if Prelude.length mergedStmts > 1
                then return $ [DN orNode, DE edge] ++ concat prereqStmts
                else return []
        else do
            prereqStmts <- mapM (reqToStmts' options parentID) reqs
            return $ concat prereqStmts
-- A prerequisite with a grade requirement.
reqToStmts' options parentID (GRADE description req) = do
    if includeGrades options then do 
        gradeNode <- makeNode (pack description)
        edge <- makeEdge (nodeID gradeNode) parentID
        prereqStmts <- reqToStmts' options (nodeID gradeNode) req
        return $ [DN gradeNode, DE edge] ++ prereqStmts
    else reqToStmts' options parentID req
-- A raw string description of a prerequisite.
reqToStmts' options parentID (RAW rawText) =
    if not (includeRaws options) || "High school" `isInfixOf` pack rawText || rawText == ""
        then return []
        else do
            prereq <- makeNode (pack rawText)
            edge <- makeEdge (nodeID prereq) parentID
            return [DN prereq, DE edge]
--A prerequisite concerning a given number of earned credits
reqToStmts' options parentID (FCES creds req) = do
    fceNode <- makeNode (pack $ "at least " ++ creds ++ " FCEs")
    edge <- makeEdge (nodeID fceNode) parentID
    prereqStmts <- reqToStmts' options (nodeID fceNode) req
    return $  [DN fceNode, DE edge] ++ prereqStmts

atLeastTwoCourseReqs :: [Req] -> Bool
atLeastTwoCourseReqs reqs = Prelude.length (Prelude.filter isNotRawOrNone reqs) > 1
    where
        isNotRawOrNone (RAW _) = False
        isNotRawOrNone NONE = False
        isNotRawOrNone _ = True

prefixedByOneOf :: Text -> [Text] -> Bool 
prefixedByOneOf name = any (flip isPrefixOf name)

makeNode :: Text -> State GeneratorState (DotNode Text)
makeNode name = do
    GeneratorState i nodesMap <- State.get
    case Map.lookup name nodesMap of
        Nothing -> do
            let node = DotNode
                        (mappendTextWithCounter name i)
                        [AC.Label $ toLabelValue name]
                nodesMap' = Map.insert name node nodesMap
            State.put (GeneratorState (i + 1) nodesMap')
            return node
        Just node -> return node

makeBool :: Text -> State GeneratorState (DotNode Text)
makeBool text1 = do
    GeneratorState i nodesMap <- State.get
    State.put (GeneratorState (i + 1) nodesMap)
    return $ DotNode (mappendTextWithCounter text1 i) (AC.Label (toLabelValue text1) : ellipseAttrs)


makeEdge :: Text -> Text -> State GeneratorState (DotEdge Text)
makeEdge id1 id2 = return $ DotEdge id1 id2 []

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
    , AC.Width 0.45     -- min 0.01
    , AC.Height 0.35    -- min 0.01
    , AC.FixedSize SetNodeSize
    , A.fillColor White
    , AC.FontSize 10.0  -- min 1.0
    ]

edgeAttrs :: GlobalAttributes
edgeAttrs = EdgeAttrs [
    ArrowHead (AType [(ArrMod FilledArrow BothSides, NoArrow)])
    ]
