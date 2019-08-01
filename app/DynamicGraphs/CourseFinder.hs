{-|
    Module      : DynamicGraphs.CourseFinder
    Description : Retrieve course information needed to generate graphs.

This module contains the logic requests for information needed to
generate graphs before retrieving and structuring the necessary data.
-}
module DynamicGraphs.CourseFinder (lookupCourses) where

import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State (StateT, execStateT, modify)
import Control.Monad.Trans.Class (lift)
import Database.CourseQueries (prereqsForCourse)
import Database.Requirement (Req(..))
import WebParsing.ReqParser (parseReqs)

-- | Recursively populate the map from courses to prerequisites, excluding the
-- prerequisites of courses that have been taken and courses in OR clauses where
-- at least one of the satisfying courses has been taken.
lookupCourses ::  [String] -- ^ a list of courses that have been taken
                  -> [T.Text]
                  -> IO (Map.Map T.Text Req)
lookupCourses taken courses =
    execStateT (mapM_ (lookupCourse $ Set.fromList taken) courses) Map.empty

lookupCourse :: Set.Set String -> T.Text -> StateT (Map.Map T.Text Req) IO ()
lookupCourse taken code = do
    prereqResults <- lift $ prereqsForCourse $ T.toStrict code
    case prereqResults of
        Left _ -> return ()
        Right prereqStr -> do
            let prereqs = parseReqs (T.unpack $ T.fromStrict prereqStr)
            modify $ Map.insert code prereqs
            lookupReqs taken prereqs

lookupReqs :: Set.Set String -> Req -> StateT (Map.Map T.Text Req) IO ()
lookupReqs taken (J name _) =
    if Set.member name taken
        -- This course has been taken; we don't need its prerequisites
        then return ()
        else lookupCourse taken $ T.pack name
lookupReqs taken (AND parents) = mapM_ (lookupReqs taken) parents
lookupReqs taken (OR parents) =
    if any hasTaken parents
        -- We've taken at least one of parents, so this entire OR is satisfied
        then return ()
        else mapM_ (lookupReqs taken) parents
    where
        hasTaken :: Req -> Bool
        hasTaken (J name _) = Set.member name taken
        hasTaken _ = False
lookupReqs taken (FCES _ parent) = lookupReqs taken parent
lookupReqs taken (GRADE _ parent) = lookupReqs taken parent
-- This will catch both NONE and RAW values.
lookupReqs _ _ = return ()
