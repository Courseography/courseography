{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}

{-|
Description: Odd miscellany of functions related to the database.

This module contains a bunch of functions related to inserting information
into the database. These functions are used as helpers for the WebParsing module.

TODO: This module should be renamed, possibly put in a different location,
and/or split up.
-}

module Database.JsonParser
    (insertCourse,
     insertLec,
     insertTut,
     setTutEnrol,
     setPracEnrol) where

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
                      (manualTutorialEnrol course)
                      (manualPracticalEnrol course)
                      (prereqs course)
                      (exclusions course)
                      (breadth course)
                      (distribution course)
                      (prereqString course)
                      (coreqs course)
                      []

-- | Updates the manualTutorialEnrolment field of the given course.
setTutEnrol :: MonadIO m => T.Text -> Bool -> ReaderT SqlBackend m ()
setTutEnrol course val =
    updateWhere [CoursesCode ==. course]
                [CoursesManualTutorialEnrolment =. Just val]

-- | Updates the manualPracticalEnrolment field of the given course.
setPracEnrol :: MonadIO m => T.Text -> Bool -> ReaderT SqlBackend m ()
setPracEnrol course val =
    updateWhere [CoursesCode ==. course]
                [CoursesManualPracticalEnrolment =. Just val]

-- | Inserts a lecture into the Lectures table associated with a given
-- session and course code.
insertLec :: MonadIO m => T.Text -> T.Text -> Lecture -> ReaderT SqlBackend m ()
insertLec session code lecture =
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
insertTut :: MonadIO m => T.Text -> T.Text-> Tutorial -> ReaderT SqlBackend m ()
insertTut session code tutorial =
    insert_ $ Tutorials code
                        (tutorialSection tutorial)
                        session
                        (map Time (times tutorial))
                        (timeStr tutorial)
