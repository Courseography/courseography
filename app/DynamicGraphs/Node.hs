module DynamicGraphs.Node (lookupCourse) where

-- TODO: Rename this file
import qualified Data.Text.Lazy as T
import Database.CourseQueries (prereqsForCourse)
import Database.Requirement (Req(..))
import WebParsing.ReqParser (parseReqs)

lookupCourse :: T.Text -> IO [(T.Text, Req)]
lookupCourse code = do
    prereqResults <- prereqsForCourse $ T.toStrict code
    case prereqResults of
        Left _ -> return []
        Right prereqStr ->
            do
            let prereqs = parseReqs (T.unpack $ T.fromStrict prereqStr)
            rest <- lookupReqs prereqs
            return $ (code, prereqs) : rest

lookupReqs :: Req -> IO [(T.Text, Req)]
lookupReqs (J name _) = lookupCourse $ T.pack name
lookupReqs (AND parents) = do
    parentReqs <- mapM lookupReqs parents
    return (concat parentReqs)
lookupReqs (OR parents) = do
    parentReqs <- mapM lookupReqs parents
    return (concat parentReqs)
lookupReqs (FCES _ parent) = lookupReqs parent
lookupReqs (GRADE _ parent) = lookupReqs parent
-- This will catch both NONE and RAW values.
lookupReqs _ = return []
