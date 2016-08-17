{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
 OverloadedStrings, TypeFamilies #-}

{-|
Description: Main module for database course seeding.

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

-- | Main function for setting up the database with course information.
--
-- TODO: Probably combine seeding of Distribution and Breadth tables,
-- and split off from @parseAll@.
setupDatabase :: IO ()
setupDatabase = do setupDistributionTable
                   print "Distribution table set up"
                   setupBreadthTable
                   print "breadth table set up"
                   parseAll
                   seedVideos

-- | Sets up the Distribution table.
setupDistributionTable :: IO ()
setupDistributionTable = runSqlite databasePath $ do
    runMigration migrateAll
    insert_ $ Distribution "Humanities"
    insert_ $ Distribution "Social Sciences"
    insert_ $ Distribution "Sciences"

-- | Sets up the Breadth table.
setupBreadthTable :: IO ()
setupBreadthTable = runSqlite databasePath $ do
    runMigration migrateAll
    insert_ $ Breadth "Creative and Cultural Representations (1)"
    insert_ $ Breadth "Thought, Belief, and Behaviour (2)"
    insert_ $ Breadth "Society and Its Institutions (3)"
    insert_ $ Breadth "Living Things and Their Environment (4)"
    insert_ $ Breadth "The Physical and Mathematical Universes (5)"
    insert_ $ Breadth "No Breadth"
