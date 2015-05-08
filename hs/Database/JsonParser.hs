{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}

{-|
Description: Odd miscellany of functions related to the database.

This module contains a bunch of functions related to inserting information
into the database. These functions are used as helpers for the WebParsing module.

TODO: This module should be renamed, possibly put in a different location,
and/or split up.
-}

module Database.JsonParser (insertCourse,
                    insertLec,
                    insertTut,
                    setTutEnrol,
                    setPracEnrol,
                    dbStr,
                    fbdbStr) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Conduit.List as CL

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Maybe (fromMaybe)
import Database.Persist.Sqlite (insert_, SqlBackend, (=.), (==.), updateWhere)
import Database.Tables

-- | The name of the 'fbdatabase' that isn't actually used anymore.
fbdbStr :: T.Text
fbdbStr = "fdatabase1.sqlite3"

-- | The path to the database file, relative to hs/.
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

-- | Inserts the lectures from course into the Lectures table.
insertLectures :: MonadIO m => Course -> ReaderT SqlBackend m ()
insertLectures course =
    insertSessionLectures (f course) "F" course >>
    insertSessionLectures (s course) "S" course >>
    insertSessionLectures (y course) "Y" course

-- | Inserts the lectures from a specified section into the Lectures table.
insertSessionLectures :: MonadIO m => Maybe Session -> T.Text -> Course -> ReaderT SqlBackend m ()
insertSessionLectures Nothing sessionStr course = return ()
insertSessionLectures (Just session) sessionStr course =
    mapM_ (insertLecture sessionStr course) (lectures session)

-- | Inserts a lecture into the Lectures table.
insertLecture :: MonadIO m => T.Text -> Course -> Lecture -> ReaderT SqlBackend m ()
insertLecture session course lecture = insertLec session (name course) lecture

-- | USED BY HASKELL TIMETABLE PARSING.
-- Identical to insertLecture but takes T.Text
-- course code instead of entire course record.
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

-- | Inserts the tutorials from course into the Tutorials table.
insertTutorials :: MonadIO m => Course -> ReaderT SqlBackend m ()
insertTutorials course =
    insertSessionTutorials (f course) "F" course >>
    insertSessionTutorials (s course) "S" course >>
    insertSessionTutorials (y course) "Y" course

-- | Inserts the tutorials from a specified section into the Tutorials table.
insertSessionTutorials :: MonadIO m => Maybe Session -> T.Text -> Course -> ReaderT SqlBackend m ()
insertSessionTutorials Nothing sessionStr course = return ()
insertSessionTutorials (Just session) sessionStr course =
    (unless $ null (tutorials session)) $
    mapM_ (insertTutorial sessionStr course) (tutorials session)

-- | Inserts a tutorial into the Tutorials table.
insertTutorial :: MonadIO m => T.Text -> Course -> Tutorial -> ReaderT SqlBackend m ()
insertTutorial session course tutorial = insertTut session (name course) tutorial

-- | USED BY HASKELL TIMETABLE PARSING.
-- Identical to insertTutorial but takes T.Text
-- course code instead of entire course record.
insertTut :: MonadIO m => T.Text -> T.Text-> Tutorial -> ReaderT SqlBackend m ()
insertTut session code tutorial =
    insert_ $ Tutorials code
                        (tutorialSection tutorial)
                        session
                        (map Time (times tutorial))
                        (timeStr tutorial)

-- | Gets the corresponding numeric requirement from a breadth requirement
-- description. 6 indicates a parsing error. Currently not used.
getBreadthRequirement :: T.Text -> Int
getBreadthRequirement reqString
    | T.isInfixOf "5" reqString = 5
    | T.isInfixOf "4" reqString = 4
    | T.isInfixOf "3" reqString = 3
    | T.isInfixOf "2" reqString = 2
    | T.isInfixOf "1" reqString = 1
    | otherwise = 6

-- | Gets the corresponding numeric requirement from a distribution requirement
-- description. 6 indicates a parsing error. Currently not used.
getDistributionRequirement :: T.Text -> Int
getDistributionRequirement reqString
    | T.isInfixOf "This is a Science course" reqString = 3
    | T.isInfixOf "This is a Social Science course" reqString = 2
    | T.isInfixOf "This is a Humanities course" reqString = 1
    | otherwise = 6
