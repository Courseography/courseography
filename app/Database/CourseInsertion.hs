{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, ScopedTypeVariables #-}

{-|
    Module      : Database.CourseInsertion
    Description : Functions that insert/update course information in the
                  database.

This module contains a bunch of functions related to inserting information
into the database. These functions are used as helpers for the WebParsing module.
-}

module Database.CourseInsertion
    (insertCourse,
     setTutorialEnrolment,
     setPracticalEnrolment,
     saveGraphJSON) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import Happstack.Server.SimpleHTTP (Response, toResponse)
import Config (databasePath)
import Database.Persist.Class (selectKeysList, Key)
import Database.Persist.Sqlite (selectFirst, fromSqlKey, toSqlKey, insertMany_, insert_, insert, SqlPersistM, (=.), (==.), updateWhere, runSqlite)
import Database.Tables hiding (texts, shapes, paths)
import qualified Data.Aeson as Aeson

-- | Inserts SVG graph data into Texts, Shapes, and Paths tables
saveGraphJSON :: BSL.ByteString -> T.Text -> IO Response
saveGraphJSON jsonStr nameStr = do
    let jsonObj = Aeson.decode jsonStr
    case jsonObj of
        Nothing -> return $ toResponse ("Error" :: String)
        Just (SvgJSON texts shapes paths) -> do
            _ <- runSqlite databasePath $ insertGraph nameStr texts shapes paths
            return $ toResponse $ ("Success" :: String)
    where
        insertGraph :: T.Text -> [Text] -> [Shape] -> [Path] -> SqlPersistM ()
        insertGraph nameStr_ texts shapes paths = do
            gId <- insert $ Graph nameStr_ 256 256
            insertMany_ $ map (\text -> text {textGraph = gId}) texts
            insertMany_ $ map (\shape -> shape {shapeGraph = gId}) shapes
            insertMany_ $ map (\path -> path {pathGraph = gId}) paths

--contains' :: PersistEntity m => T.Text -> SqlPersistM m
--contains field query = Filter field (Left $ T.concat ["%", query, "%"]) (BackendSpecificFilter "LIKE")

-- Get Key of correspondig record in Distribution column
getDistributionKey :: Maybe T.Text -> SqlPersistM (Maybe (Key Distribution))
getDistributionKey Nothing = return Nothing
getDistributionKey (Just description_) = do
    keyListDistribution :: [Key Distribution] <- selectKeysList [ DistributionDescription ==. description_ ] []
    -- option: keyListDistribution :: [DistributionId] <- selectKeysList [ DistributionDescription `contains'` description] []
    return $ case keyListDistribution of
        [] -> Nothing
        _ -> Just (head keyListDistribution)

getBreadthKey :: Maybe T.Text -> SqlPersistM (Maybe (Key Breadth))
getBreadthKey Nothing = return Nothing
getBreadthKey (Just description_) = do
    keyListBreadth :: [Key Breadth] <- selectKeysList [ BreadthDescription ==. description_ ] []
    -- option: selectKeysList [ BreadthDescription `contains'` description] []
    return $ case keyListBreadth of
        [] -> Nothing
        _ -> Just (head keyListBreadth)

-- | Inserts course into the Courses table.
insertCourse :: Course -> SqlPersistM ()
insertCourse course = do
    maybeCourse <- selectFirst [CoursesCode ==. (name course)] []
    breadthKey <- getBreadthKey (breadth course)
    distributionKey <- getDistributionKey (distribution course)
    case maybeCourse of
        Nothing -> insert_ $ Courses (name course)
                      (title course)
                      (description course)
                      (manualTutorialEnrolment course)
                      (manualPracticalEnrolment course)
                      (prereqs course)
                      (exclusions course)
                      breadthKey
                      distributionKey
                      (prereqString course)
                      (coreqs course)
                      []

        Just _ -> return ()


-- | Updates the manualTutorialEnrolment field of the given course.
setTutorialEnrolment :: T.Text -> Bool -> SqlPersistM ()
setTutorialEnrolment course val =
    updateWhere [CoursesCode ==. course]
                [CoursesManualTutorialEnrolment =. Just val]

-- | Updates the manualPracticalEnrolment field of the given course.
setPracticalEnrolment :: T.Text -> Bool -> SqlPersistM ()
setPracticalEnrolment course val =
    updateWhere [CoursesCode ==. course]
                [CoursesManualPracticalEnrolment =. Just val]
