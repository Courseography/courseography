{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}

module Database.JsonParser (insertCourse,
                    insertLec,
                    insertTut,
                    setTutEnrol,
                    setPracEnrol,
                    dbStr,
                    fbdbStr) where

import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Maybe (fromMaybe)
import Database.Persist.Sqlite
import Database.Tables

fbdbStr :: T.Text
fbdbStr = "fdatabase1.sqlite3"

dbStr :: T.Text
dbStr = "Database/database2015.sqlite3"

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

-- | Updates the manualTutorialEnrolment field of all courses with course code course
setTutEnrol :: MonadIO m => T.Text -> Bool -> ReaderT SqlBackend m ()
setTutEnrol course val =
    updateWhere [CoursesCode ==. course]
    [CoursesManualTutorialEnrolment =. Just val]

-- | updates the manualPracticalEnrolment field of all courses with course code course
setPracEnrol :: MonadIO m => T.Text -> Bool -> ReaderT SqlBackend m ()
setPracEnrol course val = do
    updateWhere [CoursesCode ==. course] [CoursesManualPracticalEnrolment =. Just val]

-- | USED BY HASKELL TIMETABLE PARSING identical to insertLecture but takes T.Text
-- | course code instead of entire course record
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

-- | USED BY HASKELL TIMETABLE PARSING. identical to inserTutorial but takes T.Text
-- | course code instead of entire course record
insertTut :: MonadIO m => T.Text -> T.Text-> Tutorial -> ReaderT SqlBackend m ()
insertTut session code tutorial =
    insert_ $ Tutorials code
                        (tutorialSection tutorial)
                        session
                        (map Time (times tutorial))
                        (timeStr tutorial)
