{-|
Description: Helpers for interacting with the graph database. Incomplete.

This module is responsible for inserting values associated with the graphs
into the database. These functions are just helpers for the main function
in "Svg.Parser", though it is possible to put all of the data retrieval code in
here as well at some point in the future.
-}

module Svg.Database where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist.Sqlite
import Database.Tables
import qualified Data.Conduit.List as CL
import Data.Conduit
import qualified Data.Text.Internal as TI
import Config (dbStr)

-- | Insert a new graph into the database, returning the key of the new graph.
insertGraph :: String   -- ^ The title of the graph that is being inserted.
            -> IO GraphId -- ^ The unique identifier of the inserted graph.
insertGraph graphTitle =
    runSqlite dbStr $ do
        runMigration migrateAll
        insert (Graph graphTitle)

-- | Insert graph components into the database.
insertElements :: ([Path], [Shape], [Text]) -> IO ()
insertElements (paths, shapes, texts) =
    runSqlite dbStr $ do
        mapM_ insert_ shapes
        mapM_ insert_ paths
        mapM_ insert_ texts

-- | The last function is a generic helper function for running an arbitrary
-- query to the database. This should only be used for debugging purposes,
-- and in fact probably moved elsewhere.

-- | Performs a query on the database.
queryDatabase :: TI.Text -> IO ()
queryDatabase sql = runSqlite dbStr $ rawQuery sql [] $$ CL.mapM_ (liftIO . print)
