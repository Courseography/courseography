{-# LANGUAGE OverloadedStrings #-}

module DynamicGraphs.GraphGenerator
  ( sampleGraph
  , coursesToPrereqGraph
  , coursesToPrereqGraphExcluding
  , graphProfileHash
  )
  where

import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Css.Constants (nodeFontSize)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.Graph (Tree (Node))
import Data.GraphViz.Attributes as A
import Data.GraphViz.Attributes.Complete as AC
import Data.GraphViz.Types.Generalised (DotEdge (..), DotGraph (..), DotNode (..),
                                        DotStatement (..), GlobalAttributes (..))
import Data.Hash.MD5 (Str (Str), md5s)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence as Seq
import Data.Text.Lazy (Text, isInfixOf, isPrefixOf, last, pack, take)
import Database.Requirement (Modifier (..), Req (..))
import DynamicGraphs.CourseFinder (lookupCourses)
import DynamicGraphs.GraphNodeUtils (formatModOr, maybeHead, paddingSpaces, stringifyModAnd)
import DynamicGraphs.GraphOptions (GraphOptions (..), defaultGraphOptions)
import Prelude hiding (last)

-- | Generates a DotGraph dependency graph including all the given courses and their recursive dependecies
coursesToPrereqGraph :: [String] -- ^ courses to generate
                        -> IO (DotGraph Text)
coursesToPrereqGraph rootCourses = coursesToPrereqGraphExcluding (map pack rootCourses) defaultGraphOptions

-- | Takes a list of courses we wish to generate a dependency graph for, along with graph options
-- for the courses we want to include. The generated graph will not contain the dependencies of the courses
-- from excluded departments. In addition, it will neither include any of the taken courses,
-- nor the dependencies of taken courses (unless they are depended on by other courses)
coursesToPrereqGraphExcluding :: [Text] -> GraphOptions -> IO (DotGraph Text)
coursesToPrereqGraphExcluding rootCourses options = do
    reqs <- lookupCourses options rootCourses
    let reqs' = Map.toList reqs
    return $ fst $ State.runState (reqsToGraph options reqs') initialState
    where
        initialState = GeneratorState 0 Map.empty

sampleGraph :: DotGraph Text
sampleGraph = fst $ State.runState (reqsToGraph
    defaultGraphOptions
    [("MAT237H1", J "MAT137H1" ""),
    ("MAT133H1", None),
    ("CSC148H1", ReqAnd [J "CSC108H1" "", J "CSC104H1" ""]),
    ("CSC265H1", ReqAnd [J "CSC148H1" "", J "CSC236H1" ""])
    ])
    (GeneratorState 0 Map.empty)

-- ** Main algorithm for converting requirements into a DotGraph

-- | Convert a list of coursenames and requirements to a DotGraph object for
--  drawing using Dot. Also prunes any repeated edges that arise from
--  multiple Reqs using the same Grade requirement
reqsToGraph :: GraphOptions -> [(Text, Req)] -> State GeneratorState (DotGraph Text)
reqsToGraph options reqs = do
    allStmts <- concatUnique <$> mapM (reqToStmts options) reqs
    return $ buildGraph allStmts
    where
        concatUnique = nubOrd . concat

data GeneratorState = GeneratorState Integer (Map.Map Text (DotNode Text))

pickCourse :: GraphOptions -> Text -> Bool
pickCourse options name =
    pickCourseByDepartment options name &&
    pickCourseByLocation options name

pickCourseByDepartment :: GraphOptions -> Text -> Bool
pickCourseByDepartment options name =
    Prelude.null (departments options) ||
    prefixedByOneOf name (departments options)

pickCourseByLocation :: GraphOptions -> Text -> Bool
pickCourseByLocation options name =
    Prelude.null (location options) ||
    courseLocation `elem` mapMaybe locationNum (location options)
    where
        courseLocation = last name
        locationNum l = case l of
            "utsg" -> Just '1'
            "utsc" -> Just '3'
            "utm" -> Just '5'
            _ -> Nothing

nodeColor :: GraphOptions -> Text -> Color
nodeColor options name = colors !! depIndex
    where colors :: [Color]
          colors = cycle $ map toColor
            [Orchid, Orange, CornFlowerBlue, Salmon, Aquamarine, Yellow, OliveDrab]
          depIndex :: Int
          depIndex = fromMaybe 0 (elemIndex courseDep (departments options))
          courseDep :: Text
          courseDep = Data.Text.Lazy.take 3 name

-- | Convert the original requirement data into dot statements that can be used by buildGraph to create the
-- corresponding DotGraph objects.
reqToStmts :: GraphOptions -> (Text, Req) -> State GeneratorState [DotStatement Text]
reqToStmts options (name, req) = do
    if pickCourse options name
        then do
            node <- makeNode name $ Just (nodeColor options name)
            stmts <- reqToStmtsTree options (nodeID node) req
            return $ DN node:concat (toList stmts)
        else return []

reqToStmtsTree :: GraphOptions -- ^ Options to toggle dynamic graph
               -> Text -- ^ Name of parent course
               -> Req  -- ^ Requirement to generate dep tree for
               -> State GeneratorState (Tree [DotStatement Text])
reqToStmtsTree _ _ None = return (Node [] [])
reqToStmtsTree options parentID (J name2 _) = do
    let name = pack name2
    if pickCourse options name then do
        prereq <- makeNode name $ Just (nodeColor options name)
        edge <- makeEdge (nodeID prereq) parentID Nothing
        return (Node [DN prereq, DE edge] [])
    else
        return (Node [] [])
-- Two or more required prerequisites.
reqToStmtsTree options parentID (ReqAnd reqs) = do
    andNode <- makeBool "and"
    edge <- makeEdge (nodeID andNode) parentID Nothing
    prereqStmts <- mapM (reqToStmtsTree options (nodeID andNode)) reqs
    let filteredStmts = Prelude.filter (Node [] [] /=) prereqStmts
    case filteredStmts of
        [] -> return $ Node [] []
        [Node (DN node:_) xs] -> do
            -- make new edge with parent id and single child id
            newEdge <- makeEdge (nodeID node) parentID Nothing
            return $ Node [DN node, DE newEdge] xs
        _ -> return $ Node [DN andNode, DE edge] filteredStmts
-- A choice from two or more prerequisites.
reqToStmtsTree options parentID (ReqOr reqs) = do
    orNode <- makeBool "or"
    edge <- makeEdge (nodeID orNode) parentID Nothing
    prereqStmts <- mapM (reqToStmtsTree options (nodeID orNode)) reqs
    let filteredStmts = Prelude.filter (Node [] [] /=) prereqStmts
    case filteredStmts of
        [] -> return $ Node [] []
        [Node (DN node:_) xs] -> do
            -- make new edge with parent id and single child id
            newEdge <- makeEdge (nodeID node) parentID Nothing
            return $ Node [DN node, DE newEdge] xs
        _  -> return $ Node [DN orNode, DE edge] filteredStmts

-- A prerequisite with a grade requirement.
reqToStmtsTree options parentID (Grade description req) = do
    if includeGrades options then do
        Node root rest <- reqToStmtsTree options parentID req
        case root of
            DN gradeNode:_ -> do
                -- make an annotated edge
                gradeEdge <- makeEdge (nodeID gradeNode)
                                      parentID
                                      (Just $ pack $ description ++ "%")
                -- swap out top edge of prereqStmt tree with annotated edge
                return $ Node [DN gradeNode, DE gradeEdge] rest
            _ -> return $ Node [] [] -- ERROR
    else reqToStmtsTree options parentID req

-- A raw string description of a prerequisite.
reqToStmtsTree options parentID (Raw rawText) =
    if not (includeRaws options) || "High school" `isInfixOf` pack rawText || rawText == ""
        then return $ Node [] []
        else do
            prereq <- makeNode (pack rawText) Nothing
            edge <- makeEdge (nodeID prereq) parentID Nothing
            return $ Node [DN prereq, DE edge] []

--A prerequisite concerning a given number of earned credits
reqToStmtsTree _ parentID (Fces creds (Requirement (Raw ""))) = do
    fceNode <- makeNode (pack $ show creds ++ " FCEs") Nothing
    edge <- makeEdge (nodeID fceNode) parentID Nothing
    return $ Node [DN fceNode, DE edge] []

--A prerequisite concerning a given number of earned credits in some raw string
reqToStmtsTree _ parentID (Fces creds (Requirement (Raw text))) = do
    fceNode <- makeNode (pack $ show creds ++ " FCEs from " ++ text ++ paddingSpaces 18) Nothing
    edge <- makeEdge (nodeID fceNode) parentID Nothing
    return $ Node [DN fceNode, DE edge] []

--A prerequisite concerning a given number of earned credits in some course(s)
reqToStmtsTree options parentID (Fces creds (Requirement req)) = do
    fceNode <- makeNode (pack $ show creds ++ " FCEs") Nothing
    edge <- makeEdge (nodeID fceNode) parentID Nothing
    prereqStmts <- reqToStmtsTree options (nodeID fceNode) req
    return $ Node [DN fceNode, DE edge] [prereqStmts]

--A prerequisite concerning a given number of earned credits in a department
reqToStmtsTree _ parentID (Fces creds (Department dept)) = do
    fceNode <- makeNode (pack $ show creds ++ " " ++ dept ++ " FCEs") Nothing
    edge <- makeEdge (nodeID fceNode) parentID Nothing
    return $ Node [DN fceNode, DE edge] []

--A prerequisite concerning a given number of earned credits at a given level
reqToStmtsTree _ parentID (Fces creds (Level level)) = do
    fceNode <- makeNode (pack $ show creds ++ " FCEs at the " ++ level ++ " level" ++ paddingSpaces 18) Nothing
    edge <- makeEdge (nodeID fceNode) parentID Nothing
    return $ Node [DN fceNode, DE edge] []

-- | A prerequisite concerning a given number of earned credits with a combination
-- | of some modifiers related through ModAnds
-- | Assumes each modifier constructor appears in modifiers at most once
-- | The ModOr constructor may appear more than once, but each occurrence
-- | of ModOr contains exactly one constructor for all its elements
-- | and such constructor does not appear anywhere else in ModAnd
reqToStmtsTree options parentID (Fces creds (ModAnd modifiers)) = do
    fceNode <- makeNode (pack $ stringifyModAnd creds modifiers ++ paddingSpaces 10) Nothing
    edge <- makeEdge (nodeID fceNode) parentID Nothing

    case maybeHead [req | Requirement req <- modifiers] of
        Nothing -> return $ Node [DN fceNode, DE edge] []
        Just req -> do
            prereqStmts <- reqToStmtsTree options (nodeID fceNode) req
            return $ Node [DN fceNode, DE edge] [prereqStmts]

-- | A prerequisite concerning a given number of earned credits with a combination
-- | of some modifiers related through a ModOr
-- | Assumes all modifiers in the list have the same constructor
reqToStmtsTree options parentID (Fces creds (ModOr modifiers)) = do
    fceNode <- makeNode (pack $ formatModOr creds modifiers) Nothing
    edge <- makeEdge (nodeID fceNode) parentID Nothing

    case maybeHead [req | Requirement req <- modifiers] of
        Nothing -> return $ Node [DN fceNode, DE edge] []
        Just req -> do
            prereqStmts <- reqToStmtsTree options (nodeID fceNode) req
            return $ Node [DN fceNode, DE edge] [prereqStmts]

-- A program requirement
reqToStmtsTree _ parentID (Program prog) = do
    -- FIXME: weird width calculation from the library with the prog
    -- so we padded the string with prog again to work around it
    progNode <- makeNode (pack $ "Enrolled in " ++ prog ++ Prelude.replicate (Prelude.length prog) ' ') Nothing
    edge <- makeEdge (nodeID progNode) parentID Nothing
    return $ Node [DN progNode, DE edge] []

-- a cGPA requirement
reqToStmtsTree _ parentID (Gpa float string) = do
    gpaNode <- makeNode (pack $ "Minimum cGPA of " ++ show float ++ string) Nothing
    edge <-  makeEdge (nodeID gpaNode) parentID Nothing
    return $ Node [DN gpaNode, DE edge] []

prefixedByOneOf :: Text -> [Text] -> Bool
prefixedByOneOf name = any (`isPrefixOf` name)

makeNode :: Text -> Maybe Color -> State GeneratorState (DotNode Text)
makeNode name nodeCol = do
    GeneratorState i nodesMap <- State.get
    case Map.lookup name nodesMap of
        Nothing -> do
            let nodeId = mappendTextWithCounter name i
                actualColor = case nodeCol of
                    Nothing -> toColor Gray
                    Just c -> c
                node = DotNode nodeId
                               [AC.Label $ toLabelValue name,
                                ID nodeId,
                                AC.FixedSize AC.GrowAsNeeded,
                                AC.FontSize nodeFontSize,
                                FillColor $ toColorList [actualColor]]
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

-- | Create edge from two node ids. Also allow for potential edge label
makeEdge :: Text -> Text -> Maybe Text -> State GeneratorState (DotEdge Text)
makeEdge id1 id2 description =
    return $ DotEdge id1 id2
                     (ID (id1 `mappend` "|" `mappend` id2) : textLabelList)
    where
        textLabelList = case description of
            Nothing -> []
            Just a -> [textLabel a]

mappendTextWithCounter :: Text -> Integer -> Text
mappendTextWithCounter text1 counter = text1 `mappend` "_counter_" `mappend` pack (show counter)

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
