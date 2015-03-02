{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}


module Database.JsonParser (insertCourse, 
                    dbStr,
                    encodeJSON) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import Data.Aeson
import Data.List as L
import GHC.Generics
import System.Directory
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Applicative
import Data.Maybe
import Database.Tables

dbStr :: T.Text
dbStr = "Database/database1.sqlite3"

courseDirectory :: String
courseDirectory = "../../res/courses/"

-- | Opens a directory contained in dir, and processes every file in that directory.
processDirectory :: IO ()
processDirectory = do
    contents <- getDirectoryContents courseDirectory
    let formattedContents = map (courseDirectory ++) (L.sort contents)
    filterM doesFileExist formattedContents >>= mapM_ printFile

-- | Opens and reads a files contents, and decodes JSON content into a Course data structure.
printFile :: String -> IO ()
printFile courseFile = 
    do d <- eitherDecode <$> getJSON courseFile
       case d of
           Left err -> print $ courseFile ++ " " ++ err
           Right course -> do
                runSqlite dbStr $ do
                    runMigration migrateAll
                    insertCourse course
                    insertLectures course
                    insertTutorials course
                    liftIO $ print $ "Inserted " ++ show (name course)

-- | Opens and reads the file contained in `jsonFile`. File contents are returned, surrounded by
-- | square brackets.
getJSON :: String -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

-- | Inserts course into the Courses table.
insertCourse :: MonadIO m => Course -> ReaderT SqlBackend m ()
insertCourse course =
    insert_ $ Courses (name course)
                      (title course)
                      (description course)
                      (manualTutorialEnrol course)
                      (prereqString course)
                      (exclusions course)
                      (breadth course)
                      (distribution course)
                      (prereqString course)

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
insertLecture session course lecture =
    insert_ $ Lectures (name course)
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
insertTutorial session course tutorial = 
    insert_ $ Tutorials (name course)
                        (tutorialSection tutorial)
                        session
                        (map Time (times tutorial))
                        (timeStr tutorial)

-- | Gets the corresponding numeric requirement from a breadth requirement description.
-- | 6 indicates a parsing error.
getBreadthRequirement :: T.Text -> Int
getBreadthRequirement reqString
    | T.isInfixOf "5" reqString = 5
    | T.isInfixOf "4" reqString = 4
    | T.isInfixOf "3" reqString = 3
    | T.isInfixOf "2" reqString = 2
    | T.isInfixOf "1" reqString = 1
    | otherwise = 6

-- | Gets the corresponding numeric requirement from a distribution requirement description.
-- | 6 indicates a parsing error.
getDistributionRequirement :: T.Text -> Int
getDistributionRequirement reqString
    | T.isInfixOf "This is a Science course" reqString = 3
    | T.isInfixOf "This is a Social Science course" reqString = 2
    | T.isInfixOf "This is a Humanities course" reqString = 1
    | otherwise = 6

-- | Encodes an Aeson Value into a ByteString.
encodeJSON :: Value -> BSL.ByteString
encodeJSON json = BSL.filter (\c -> c /= '\\') $ encode json
