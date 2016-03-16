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
import Happstack.Server.SimpleHTTP
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Maybe (fromMaybe)
import Database.Persist.Sqlite (insert_, SqlBackend, (=.), (==.), updateWhere)
import Util.Happstack (createJSONResponse)
import Database.Tables
import Data.Aeson

-- | Inserts SVG graph data into Texts, Shapes, and Paths tables
-- saveGraphJSON :: [Value] -> ()
saveGraphJSON :: String -> IO Response
saveGraphJSON jsonStr = do
    return $ createJSONResponse jsonStr

-- | Inserts course into the Courses table.
insertCourse :: MonadIO m => Course -> ReaderT SqlBackend m ()
insertCourse course =
    insert_ $ Courses (name course)
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

