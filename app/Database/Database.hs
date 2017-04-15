{-|
    Module      : Database.Database
    Description : Main module for database course seeding.

The main module for parsing course information from the web and
inserting it into the database. Run when @cabal run database@ is executed.
-}

module Database.Database
    (setupDatabase) where

import Database.Persist.Sqlite (runSqlite, runMigration, insert_)
import Database.Tables
import WebParsing.ParseAll (parseAll)
import Database.CourseVideoSeed (seedVideos)
import Config (databasePath)
import System.Directory (createDirectoryIfMissing)
import Data.Text as T (length, findIndex, take, unpack, reverse)
import Data.Maybe (fromMaybe)


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
