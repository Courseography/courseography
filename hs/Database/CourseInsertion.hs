{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}

{-|
Description: Functions that insert/update course information in the database.

This module contains a bunch of functions related to inserting information
into the database. These functions are used as helpers for the WebParsing module.
-}

module Database.CourseInsertion
    (insertCourse,
     insertLecture,
     insertTutorial,
     setTutorialEnrolment,
     setPracticalEnrolment) where

import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Maybe (fromMaybe)

import Database.Persist.Sqlite (insert_, SqlBackend, (=.), (==.), updateWhere)
import Database.Tables

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

-- | Inserts a lecture into the Lectures table associated with a given
-- session and course code.
insertLecture :: MonadIO m => T.Text -> T.Text -> Lecture -> ReaderT SqlBackend m ()
insertLecture session code lecture =
    insert_ $ Lectures code
                       session
                       (section lecture)
                       (map Time (time lecture))
                       (cap lecture)
                       (instructor lecture)
                       (fromMaybe 0 (enrol lecture))
                       (fromMaybe 0 (wait lecture))
                       (extra lecture)
                       (time_str lecture)

-- | Inserts a tutorial into the Tutorials table associated with a given
-- session and course code.
insertTutorial :: MonadIO m => T.Text -> T.Text-> Tutorial -> ReaderT SqlBackend m ()
insertTutorial session code tutorial =
    insert_ $ Tutorials code
                        (tutorialSection tutorial)
                        session
                        (map Time (times tutorial))
                        (timeStr tutorial)
