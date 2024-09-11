{-|
    Module      : Database.CourseInsertion
    Description : Functions that insert/update course information in the
                  database.

This module contains a bunch of functions related to inserting information
into the database. These functions are used as helpers for the WebParsing module.
-}

module Database.CourseInsertion
    (insertCourse,
     saveGraphJSON) where

import Config (databasePath)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Database.Persist.Class (selectKeysList)
import Database.Persist.Sqlite (SqlPersistM, insert, insertMany_, insert_, runSqlite, selectFirst,
                                (==.))
import Database.Tables hiding (breadth, distribution, paths, shapes, texts)
import Happstack.Server(lookBS, lookText', ServerPart, Response, toResponse)

-- | Inserts SVG graph data into Texts, Shapes, and Paths tables
saveGraphJSON :: ServerPart Response
saveGraphJSON = do
    jsonStr <- lookBS "jsonData"
    nameStr <- lookText' "nameData"
    let jsonObj = Aeson.decode jsonStr :: Maybe SvgJSON
    case jsonObj of
        Nothing -> return $ toResponse ("Error" :: String)
        Just (SvgJSON texts shapes paths) -> do
            _ <- liftIO $ runSqlite databasePath $ insertGraph nameStr texts shapes paths
            return $ toResponse ("Success" :: String)
    where
        insertGraph :: T.Text -> [Text] -> [Shape] -> [Path] -> SqlPersistM ()
        insertGraph nameStr_ texts shapes paths = do
            gId <- insert $ Graph nameStr_ 256 256 False
            insertMany_ $ map (\text -> text {textGraph = gId}) texts
            insertMany_ $ map (\shape -> shape {shapeGraph = gId}) shapes
            insertMany_ $ map (\path -> path {pathGraph = gId}) paths

--contains' :: PersistEntity m => T.Text -> SqlPersistM m
--contains field query = Filter field (Left $ T.concat ["%", query, "%"]) (BackendSpecificFilter "LIKE")

-- Get Key of correspondig record in Distribution column
getDistributionKey :: T.Text -> SqlPersistM (Maybe (Key Distribution))
getDistributionKey description_ = do
    keyListDistribution :: [Key Distribution] <- selectKeysList [ DistributionDescription ==. description_ ] []
    -- option: keyListDistribution :: [DistributionId] <- selectKeysList [ DistributionDescription `contains'` description] []
    return $ case keyListDistribution of
        [] -> Nothing
        _ -> Just (head keyListDistribution)

getBreadthKey :: T.Text -> SqlPersistM (Maybe (Key Breadth))
getBreadthKey description_ = do
    keyListBreadth :: [Key Breadth] <- selectKeysList [ BreadthDescription ==. description_ ] []
    -- option: selectKeysList [ BreadthDescription `contains'` description] []
    return $ case keyListBreadth of
        [] -> Nothing
        _ -> Just (head keyListBreadth)

-- | Inserts course into the Courses table.
insertCourse :: (Courses, T.Text, T.Text) -> SqlPersistM ()
insertCourse (course, breadth, distribution) = do
    maybeCourse <- selectFirst [CoursesCode ==. coursesCode course] []
    breadthKey <- getBreadthKey breadth
    distributionKey <- getDistributionKey distribution
    case maybeCourse of
        Nothing -> insert_ $ course {coursesBreadth = breadthKey,
                                     coursesDistribution = distributionKey}
        Just _ -> return ()
