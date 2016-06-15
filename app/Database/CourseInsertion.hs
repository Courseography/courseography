{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}

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
import Happstack.Server.SimpleHTTP
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (when) --Kael 
import Control.Monad.Trans.Reader (ReaderT)
import Data.Maybe (fromMaybe) 
import Config (databasePath)
import Database.Persist -- Kael
import Database.Persist.Sqlite (selectFirst, fromSqlKey, toSqlKey, insertMany_, insert_, insert, repsert, SqlBackend, (=.), (==.), updateWhere, runSqlite)
import Database.Tables
import Data.Aeson
import Control.Monad.IO.Class (liftIO) -- Kael
import Database.Persist.Sqlite (get) -- Kael

-- | Inserts SVG graph data into Texts, Shapes, and Paths tables
saveGraphJSON :: String -> String -> IO Response
saveGraphJSON jsonStr nameStr = do
    let jsonObj = decode $ BSL.pack jsonStr
    case jsonObj of
        Nothing -> return $ toResponse ("Error" :: String)
        Just (SvgJSON texts shapes paths) -> do
            response <- runSqlite databasePath $ do
                gId <- insert $ Graph nameStr 256 256
                insertMany_ $ map (\text -> text {textGraph = gId}) texts
                insertMany_ $ map (\shape -> shape {shapeGraph = gId}) shapes
                insertMany_ $ map (\path -> path {pathGraph = gId}) paths
            return $ toResponse $ ("Success" :: String)

-- | Check for duplicate key, then if unique, insert course into Courses table
safeInsertCourse course = do
    maybeCourse <- selectFirst [CoursesCode ==. (name course)] [] -- maybeCourse :: [Entity Courses]
    case maybeCourse of
        Nothing -> insertCourse course
        Just _ -> return ()


--newCourse c = do 
--    case existingCourse of
--        ReaderT SqlBackend m0 Nothing -> True
--        _ -> False
    --where existingCourse = get (CoursesKey c) 

-- | Inserts course into the Courses  table.
insertCourse :: MonadIO m => Course -> ReaderT SqlBackend m ()
insertCourse course = do 
    liftIO $ print $ (name course)
    insert_ $ Courses (name course) -- idea: insertKey?
                      (title course)
                      (description course)
                      (manualTutorialEnrolment course)
                      (manualPracticalEnrolment course)
                      (prereqs course)
                      (exclusions course)
                      (breadth course)
                      (distribution course)
                      (prereqString course)
                      (coreqs course)
                      []
      
-- | Updates the manualTutorialEnrolment field of the given course.
setTutorialEnrolment :: MonadIO m => T.Text -> Bool -> ReaderT SqlBackend m ()
setTutorialEnrolment course val =
    updateWhere [CoursesCode ==. course]
                [CoursesManualTutorialEnrolment =. Just val]

-- | Updates the manualPracticalEnrolment field of the given course.
setPracticalEnrolment :: MonadIO m => T.Text -> Bool -> ReaderT SqlBackend m ()
setPracticalEnrolment course val =
    updateWhere [CoursesCode ==. course]
                [CoursesManualPracticalEnrolment =. Just val]

