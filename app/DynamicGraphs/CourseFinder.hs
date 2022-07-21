{-|
    Module      : DynamicGraphs.CourseFinder
    Description : Retrieve course information needed to generate graphs.

This module contains the logic requests for information needed to
generate graphs before retrieving and structuring the necessary data.
-}
module DynamicGraphs.CourseFinder (lookupCourses) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT, modify)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import Database.CourseQueries (prereqsForCourse)
import Database.Requirement (Modifier (..), Req (..))
import DynamicGraphs.GraphOptions (GraphOptions (..))
import WebParsing.ReqParser (parseReqs)

lookupCourses :: GraphOptions -> [T.Text] -> IO (Map.Map T.Text Req)
lookupCourses options courses =
    execStateT (mapM_ (lookupCourse options) courses) Map.empty

lookupCourse :: GraphOptions -> T.Text -> StateT (Map.Map T.Text Req) IO ()
lookupCourse options code = do
    prereqResults <- lift $ prereqsForCourse $ T.toStrict code
    case prereqResults of
        Left _ -> return ()
        Right prereqStr -> do
            let prereqs = parseReqs (T.unpack $ T.fromStrict prereqStr)
            modify $ Map.insert code prereqs
            lookupReqs options prereqs

lookupReqs :: GraphOptions -> Req -> StateT (Map.Map T.Text Req) IO ()
lookupReqs options (J name _) = do
    if Set.member name (Set.fromList $ map T.unpack (taken options)) ||
        not (Set.member (take 3 name) $ Set.fromList $ map T.unpack (departments options))
        -- This course has been taken or is not a department we want to include; we don't need its prerequisites
        then return ()
        else lookupCourse options $ T.pack name
lookupReqs options (ReqAnd parents) = mapM_ (lookupReqs options) parents
lookupReqs options (ReqOr parents) =
    if any hasTaken parents
        -- We've taken at least one of parents, so this entire ReqOr is satisfied
        then return ()
        else mapM_ (lookupReqs options) parents
    where
        hasTaken :: Req -> Bool
        hasTaken (J name _) = Set.member name (Set.fromList $ map T.unpack (taken options))
        hasTaken _ = False
lookupReqs options (Fces _ (Requirement parent)) = lookupReqs options parent
lookupReqs options (Grade _ parent) = lookupReqs options parent
-- This will catch None, Raw, and Fces with non-course modifiers
lookupReqs _ _ = return ()
