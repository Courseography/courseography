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

import Control.Monad
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

-- | Inserts the lectures from course into the Lectures table.
--insertLectures :: MonadIO m => Course -> ReaderT SqlBackend m ()
--insertLectures course =
--    insertSessionLectures (f course) "F" course >>
--    insertSessionLectures (s course) "S" course >>
--    insertSessionLectures (y course) "Y" course

-- | Inserts the lectures from a specified section into the Lectures table.
--insertSessionLectures :: MonadIO m => Maybe Session -> T.Text -> Course -> ReaderT SqlBackend m ()
--insertSessionLectures Nothing sessionStr course = return ()
--insertSessionLectures (Just session) sessionStr course =
--    mapM_ (insertLecture sessionStr course) (lectures session)

-- | Inserts a lecture into the Lectures table.
--insertLecture :: MonadIO m => T.Text -> Course -> Lecture -> ReaderT SqlBackend m ()
--insertLecture session course lecture =
--    insert_ $ Lectures (name course)
--                       session
--                       (section lecture)
--                       (map Time (time lecture))
--                       (cap lecture)
--                       (instructor lecture)
--                       (fromMaybe 0 (enrol lecture))
--                       (fromMaybe 0 (wait lecture))
--                       (extra lecture)
--                       (time_str lecture)

-- | Inserts the tutorials from course into the Tutorials table.
--insertTutorials :: MonadIO m => Course -> ReaderT SqlBackend m ()
--insertTutorials course =
--    insertSessionTutorials (f course) "F" course >>
--    insertSessionTutorials (s course) "S" course >>
--    insertSessionTutorials (y course) "Y" course

-- | Inserts the tutorials from a specified section into the Tutorials table.
--insertSessionTutorials :: MonadIO m => Maybe Session -> T.Text -> Course -> ReaderT SqlBackend m ()
--insertSessionTutorials Nothing sessionStr course = return ()
--insertSessionTutorials (Just session) sessionStr course =
--    (unless $ null (tutorials session)) $
--    mapM_ (insertTutorial sessionStr course) (tutorials session)

-- | Inserts a tutorial into the Tutorials table.
--insertTutorial :: MonadIO m => T.Text -> Course -> Tutorial -> ReaderT SqlBackend m ()
--insertTutorial session course tutorial =
--    insert_ $ Tutorials (name course)
--                        (tutorialSection tutorial)
--                        session
--                        (map Time (times tutorial))
--                        (timeStr tutorial)

-- | Gets the corresponding numeric requirement from a breadth requirement description.
-- | 6 indicates a parsing error.
--getBreadthRequirement :: T.Text -> Int
--getBreadthRequirement reqString
--    | T.isInfixOf "5" reqString = 5
--    | T.isInfixOf "4" reqString = 4
--    | T.isInfixOf "3" reqString = 3
--    | T.isInfixOf "2" reqString = 2
--    | T.isInfixOf "1" reqString = 1
--    | otherwise = 6

-- | Gets the corresponding numeric requirement from a distribution requirement description.
-- | 6 indicates a parsing error.
--getDistributionRequirement :: T.Text -> Int
--getDistributionRequirement reqString
--    | T.isInfixOf "This is a Science course" reqString = 3
--    | T.isInfixOf "This is a Social Science course" reqString = 2
--    | T.isInfixOf "This is a Humanities course" reqString = 1
--    | otherwise = 6
