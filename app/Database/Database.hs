{-|
    Module      : Database.Database
    Description : Main module for database course seeding.

The main module for parsing course information from the web and
inserting it into the database. Run when @cabal run database@ is executed.
-}

module Database.Database
    (populateCalendar, setupDatabase) where

import Config (databasePath, runDb)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text as T (findIndex, length, reverse, take, unpack)
import Database.CourseVideoSeed (seedVideos)
import Database.Persist.Sqlite (insert_, runMigration)
import Database.Tables
import System.Directory (createDirectoryIfMissing)
import WebParsing.ArtSciParser (parseCalendar)


distTableSetUpStr :: String
distTableSetUpStr = "Distribution table set up"
breathTableSetUpStr :: String
breathTableSetUpStr = "breadth table set up"


-- | Creates the database if it doesn't exist and runs migrations.
setupDatabase :: IO ()
setupDatabase = do
      -- Create db folder if it doesn't exist
      dbPath <- liftIO databasePath
      let ind = (T.length dbPath -) . fromMaybe 0 . T.findIndex (=='/') . T.reverse $ dbPath
          db = T.unpack $ T.take ind dbPath
      createDirectoryIfMissing True db
      runDb $ runMigration migrateAll

-- | Sets up the course information from Artsci Calendar
populateCalendar :: IO ()
populateCalendar = do
    populateStaticInfo
    parseCalendar

-- | Sets up the tables and seeds the videos for the database.
populateStaticInfo :: IO ()
populateStaticInfo = do
    setupDistributionTable
    print distTableSetUpStr
    setupBreadthTable
    print breathTableSetUpStr
    seedVideos

-- | Sets up the Distribution table.
setupDistributionTable :: IO ()
setupDistributionTable = runDb $ do
    insert_ $ Distribution "Humanities"
    insert_ $ Distribution "Social Science"
    insert_ $ Distribution "Science"

-- | Sets up the Breadth table.
setupBreadthTable :: IO ()
setupBreadthTable = runDb $ do
    insert_ $ Breadth "Creative and Cultural Representations (1)"
    insert_ $ Breadth "Thought, Belief, and Behaviour (2)"
    insert_ $ Breadth "Society and its Institutions (3)"
    insert_ $ Breadth "Living Things and Their Environment (4)"
    insert_ $ Breadth "The Physical and Mathematical Universes (5)"
    insert_ $ Breadth "No Breadth"
