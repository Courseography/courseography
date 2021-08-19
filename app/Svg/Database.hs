{-|
Description: Helpers for interacting with the graph database. Incomplete.

This module is responsible for inserting values associated with the graphs
into the database. These functions are just helpers for the main function
in "Svg.Parser", though it is possible to put all of the data retrieval code in
here as well at some point in the future.
-}

module Svg.Database
    (insertGraph, insertElements, deleteGraphs) where

import qualified Data.Text as T
import Database.Persist.Sqlite
import Database.Tables hiding (graphDynamic, graphHeight, graphWidth, paths, shapes, texts)

-- | Insert a new graph into the database, returning the key of the new graph.
insertGraph :: T.Text   -- ^ The title of the graph that is being inserted.
            -> Double   -- ^ The width dimension of the graph
            -> Double   -- ^ The height dimension of the graph
            -> Bool     -- ^ True if graph is dynamically generated
            -> SqlPersistM GraphId -- ^ The unique identifier of the inserted graph.
insertGraph graphName graphWidth graphHeight graphDynamic = do
    runMigration migrateAll
    insert (Graph graphName graphWidth graphHeight graphDynamic)

-- | Insert graph components into the database.
insertElements :: ([Path], [Shape], [Text]) -> SqlPersistM ()
insertElements (paths, shapes, texts) = do
    mapM_ insert_ shapes
    mapM_ insert_ paths
    mapM_ insert_ texts

-- | Delete graphs from the database.
deleteGraphs :: SqlPersistM ()
deleteGraphs = do
    runMigration migrateAll
    deleteWhere ([] :: [Filter Graph])
    deleteWhere ([] :: [Filter Text])
    deleteWhere ([] :: [Filter Shape])
    deleteWhere ([] :: [Filter Path])
