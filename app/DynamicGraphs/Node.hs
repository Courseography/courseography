module DynamicGraphs.Node (Node(..), lookupCourse) where

import qualified Data.Map.Strict as Map
import Database.CourseQueries (coursesToPrereqs)
import Database.Requirement (Req(..))
import WebParsing.ReqParser (parseReqs)

data Node = Leaf String
          | Parent String Node
          | Conj [Node]
          | Disj [Node]
          deriving (Show)

-- A map from course codes to their prerequisites as a string.
type AllPrereqs = Map.Map String String

lookupCourse :: String -> IO Node
lookupCourse code = fromCourse code <$> coursesToPrereqs

fromCourse :: String -> AllPrereqs -> Node
fromCourse code prereqs =
    case Map.lookup code prereqs of
        -- TODO: We may want to disambiguate between courses that have no
        -- prerequisites and courses that don't exist in the database.
        Nothing -> Leaf code
        Just prereqStr -> Parent code $ reqToNode prereqs (parseReqs prereqStr)

reqToNode :: AllPrereqs -> Req -> Node
reqToNode prereqs (J name _) = fromCourse name prereqs
-- TODO: Do we want to disambiguate between raw text and courses when generating
-- graphs?
reqToNode _ (RAW text) = Leaf text
reqToNode prereqs (AND parents) = Conj $ map (reqToNode prereqs) parents
reqToNode prereqs (OR parents) = Disj $ map (reqToNode prereqs) parents
reqToNode prereqs (FCES _ parent) = reqToNode prereqs parent
reqToNode prereqs (GRADE _ parent) = reqToNode prereqs parent
reqToNode _ NONE = error "reqToNode encountered NONE value"
