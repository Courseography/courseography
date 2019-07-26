{-|
    Module      : DynamicGraphs.CourseFinder
    Description : Retrieve course information needed to generate graphs.

This module contains the logic requests for information needed to
generate graphs before retrieving and structuring the necessary data.
-}
module DynamicGraphs.CourseFinder (lookupCourses) where

import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Database.CourseQueries (prereqsForCourse)
import Database.Requirement (Req(..))
import WebParsing.ReqParser (parseReqs)

-- | Return a map from course codes to prerequisites for the given courses and
-- all of their prerequisites, followed by their prerequisites' prerequisites,
-- and so on.
lookupCourses :: [T.Text] -> IO (Map.Map T.Text Req)
lookupCourses courses = execStateT (mapM_ lookupCourse courses) Map.empty

lookupCourse :: T.Text -> StateT (Map.Map T.Text Req) IO ()
lookupCourse code = do
    prereqResults <- lift $ prereqsForCourse $ T.toStrict code
    case prereqResults of
        Left _ -> return ()
        Right prereqStr -> do
            let prereqs = parseReqs (T.unpack $ T.fromStrict prereqStr)
            modify $ Map.insert code prereqs
            lookupReqs prereqs

lookupReqs :: Req -> StateT (Map.Map T.Text Req) IO ()
lookupReqs (J name _) = lookupCourse $ T.pack name
lookupReqs (AND parents) = mapM_ lookupReqs parents
lookupReqs (OR parents) = mapM_ lookupReqs parents
lookupReqs (FCES _ parent) = lookupReqs parent
lookupReqs (GRADE _ parent) = lookupReqs parent
-- This will catch both NONE and RAW values.
lookupReqs _ = return ()
