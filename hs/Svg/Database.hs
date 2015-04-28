{-# LANGUAGE OverloadedStrings #-}

-- |This module is responsible for inserting the graph into the database.
module Svg.Database where

import Control.Monad.IO.Class  (liftIO, MonadIO)
import Database.Persist.Sqlite
import Database.Tables
import Database.Persist
import Control.Monad.Trans.Reader
import qualified Data.Conduit.List as CL
import Data.Int
import Data.Conduit
import qualified Data.Text.Internal as TI
import Database.JsonParser

-- | Performs a query on the database.
queryDatabase :: TI.Text -> IO ()
queryDatabase sql = runSqlite dbStr $ rawQuery sql [] $$ CL.mapM_ (liftIO . print)

insertGraph :: String   -- ^ The title of the graph that is being inserted.
            -> IO Int64 -- ^ The unique identifier of the inserted graph.
insertGraph graphTitle =
    runSqlite dbStr $ do
        runMigration migrateAll
        key <- insert (Graph 0 graphTitle)
        let (PersistInt64 keyId) = toPersistValue key
        update key [GraphGId =. keyId]
        return keyId

insertElements :: ([Path], [Shape], [Text]) -> IO ()
insertElements (paths, shapes, texts) =
    runSqlite dbStr $ do
        mapM_ insert_ shapes
        mapM_ insert_ paths
        mapM_ insert_ texts