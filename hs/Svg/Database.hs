{-|
Description: Helpers for interacting with the graph database. Incomplete.

This module is responsible for inserting values associated with the graphs
into the database. These functions are just helpers for the main function
in "Svg.Parser", though it is possible to put all of the data retrieval code in
here as well at some point in the future.
-}

module Svg.Database
    (insertGraph, insertElements, deleteGraphs) where

import Database.Persist.Sqlite
import Database.Tables
import Config (databasePath)

-- | Insert a new graph into the database, returning the key of the new graph.
insertGraph :: String   -- ^ The title of the graph that is being inserted.
            -> IO GraphId -- ^ The unique identifier of the inserted graph.
insertGraph graphName =
    runSqlite databasePath $ do
        runMigration migrateAll
        insert (Graph graphName)

-- | Insert graph components into the database.
insertElements :: ([Path], [Shape], [Text]) -> IO ()
insertElements (paths, shapes, texts) =
    runSqlite databasePath $ do
        mapM_ insert_ shapes
        mapM_ insert_ paths
        mapM_ insert_ texts

-- | Delete graphs from the database.
deleteGraphs :: IO ()
deleteGraphs = runSqlite databasePath $ do
    deleteWhere ([] :: [Filter Graph])
    deleteWhere ([] :: [Filter Text])
    deleteWhere ([] :: [Filter Shape])
    deleteWhere ([] :: [Filter Path])
