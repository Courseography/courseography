{-|
    Module      : Database.Database
    Description : Main module for database course seeding.

The main module for parsing course information from the web and
inserting it into the database. Run when @cabal run database@ is executed.
-}

module Database.Database
    (parseDatabase, setupDatabase) where

import Config (databasePath)
import Data.Maybe (fromMaybe)
import Data.Text as T (findIndex, length, reverse, take, unpack)
import Database.CourseVideoSeed (seedVideos)
import Database.Persist.Sqlite (insert_, runMigration, runSqlite)
import Database.Tables
import System.Directory (createDirectoryIfMissing)
import WebParsing.ParseAll (parseAll)


distTableSetUpStr :: String
distTableSetUpStr = "Distribution table set up"
breathTableSetUpStr :: String
breathTableSetUpStr = "breadth table set up"


-- | Main function for setting up the database with course information.
--
-- TODO: Probably combine seeding of Distribution and Breadth tables,
-- and split off from @parseAll@.
setupDatabase :: IO ()
setupDatabase = do
    -- Create db folder if it doesn't exist
    let ind = (T.length databasePath -) . fromMaybe 0 . T.findIndex (=='/') . T.reverse $ databasePath
        db = T.unpack $ T.take ind databasePath
    createDirectoryIfMissing True db
    setupDistributionTable
    print distTableSetUpStr
    setupBreadthTable
    print breathTableSetUpStr

parseDatabase :: IO ()
parseDatabase = do    
    parseAll
    seedVideos

-- | Sets up the Distribution table.
setupDistributionTable :: IO ()
setupDistributionTable = runSqlite databasePath $ do
    runMigration migrateAll
    insert_ $ Distribution "Humanities"
    insert_ $ Distribution "Social Science"
    insert_ $ Distribution "Science"

-- | Sets up the Breadth table.
setupBreadthTable :: IO ()
setupBreadthTable = runSqlite databasePath $ do
    runMigration migrateAll
    insert_ $ Breadth "Creative and Cultural Representations (1)"
    insert_ $ Breadth "Thought, Belief, and Behaviour (2)"
    insert_ $ Breadth "Society and its Institutions (3)"
    insert_ $ Breadth "Living Things and Their Environment (4)"
    insert_ $ Breadth "The Physical and Mathematical Universes (5)"
    insert_ $ Breadth "No Breadth"
