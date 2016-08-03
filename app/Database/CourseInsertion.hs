{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, ScopedTypeVariables #-}

{-|
Description: Functions that insert/update course information in the database.

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
import Database.Persist.Class (selectKeysList, PersistEntity, Key) --kael
import Database.Persist.Sqlite (selectFirst, insertMany_, insert_, insert, SqlPersistM, (=.), (==.), updateWhere, runSqlite)
import Database.Tables
import qualified Data.Aeson as Aeson

-- | Inserts SVG graph data into Texts, Shapes, and Paths tables
saveGraphJSON :: String -> String -> IO Response
saveGraphJSON jsonStr nameStr = do
    let jsonObj = Aeson.decode $ BSL.pack jsonStr
    case jsonObj of
        Nothing -> return $ toResponse ("Error" :: String)
        Just (SvgJSON texts shapes paths) -> do
            _ <- runSqlite databasePath $ do
                gId <- insert $ Graph nameStr 256 256
                insertMany_ $ map (\text -> text {textGraph = gId}) texts
                insertMany_ $ map (\shape -> shape {shapeGraph = gId}) shapes
                insertMany_ $ map (\path -> path {pathGraph = gId}) paths
            return $ toResponse $ ("Success" :: String)

-- Get Key of correspondig record in Distribution column  
getDistributionKey :: Maybe T.Text -> Maybe DistributionId 
getDistributionKey description = do
    case description of
        Nothing -> Nothing
        Just _ -> do
            textDescription :: T.Text <- description
            keyListDistribution :: [DistributionId] <- selectKeysList [ DistributionDescription ==. textDescription ] [] 
            return (head keyListDistribution)
            
--alternate version:
--getDistributionKey :: Maybe T.Text -> Maybe DistributionId 
--getDistributionKey description --(equals)-- -- do
    --case description of
    --    Nothing -> Nothing
    --    Just _ -> do
    --        keyListDistribution :: [DistributionId] <- selectKeysList [ DistributionDescription ==. description ] [] 
    --        let keyDistribution --(equals)-- (if null keyListDistribution then Nothing else Just (head keyListDistribution))
  

-- **Problem: Breadth as it is hard-coded in Database.hs won't match breadth field from Code.
-- Get Key of corresponding breadth record 
getBreadthKey :: Maybe T.Text -> Maybe BreadthId 
getBreadthKey description = do
    case description of   
        Nothing -> Nothing
        Just _ -> do
            textDescription :: T.Text <- description  
            keyListBreadth :: [BreadthId] <- selectKeysList [ BreadthDescription ==. textDescription ] [] 
            case keyListBreadth of 
                null -> Nothing
                _ -> Just (head keyListBreadth)

            --if null keyListBreadth
            --then Nothing
            --else Just (head keyListBreadth)  
          -- or 
            --return (head keyListBreadth)

-- | Inserts course into the Courses table.
insertCourse :: Course -> SqlPersistM ()
insertCourse course = do
    maybeCourse <- selectFirst [CoursesCode ==. (name course)] []
    case maybeCourse of
        Nothing -> insert_ $ Courses (name course)
                      (title course)
                      (description course)
                      (manualTutorialEnrolment course)
                      (manualPracticalEnrolment course)
                      (prereqs course)
                      (exclusions course)
                      (getBreadthKey (breadth course))
                      (getDistributionKey (distribution course))
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

