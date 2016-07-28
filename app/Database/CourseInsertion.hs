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
import Happstack.Server.SimpleHTTP
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import Config (databasePath)
import Database.Persist --kael
import Database.Persist.Sqlite (selectFirst, fromSqlKey, toSqlKey, insertMany_, insert_, insert, SqlBackend, SqlPersistM, (=.), (==.), updateWhere, runSqlite)
import Database.Tables
import Data.Aeson

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
                      (breadth course)
                      (distribution course)
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

