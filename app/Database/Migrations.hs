module Database.Migrations
    (migrateDatabase, getDatabaseVersion, setDatabaseVersion, migrationList) where

import Control.Monad.Reader (MonadIO)
import Data.List (sortOn)
import Database.Persist.Sql (Entity (..), Migration, SqlPersistT, addMigration, insert_,
                             runMigrationUnsafe, selectFirst, update, (=.))
import Database.Tables

data MigrationWrapper = MigrationWrapper {
    version :: Int,
    script :: Migration
}

-- | Migrates the database
migrateDatabase :: MonadIO m => SqlPersistT m ()
migrateDatabase = do
    currVersion <- getDatabaseVersion
    applyMigrations currVersion migrationList

-- | Migrates the database by applying only migrations newer than the current version number
applyMigrations :: MonadIO m => Int -> [MigrationWrapper] -> SqlPersistT m ()
applyMigrations currVersion migrations = do
    mapM_ (runMigrationUnsafe . script)
        $ sortOn version
        $ filter (\migration -> version migration > currVersion) migrations

    case migrations of
        [] -> return ()
        _ -> setDatabaseVersion $ maximum $ map version migrations

-- | List of migrations
migrationList :: [MigrationWrapper]
migrationList = [MigrationWrapper {version=2, script=renamePostTables}]

-- | Migration script which renames the Post tables to Program
renamePostTables :: Migration
renamePostTables = do
    addMigration True "ALTER TABLE post RENAME TO program;"
    addMigration True "ALTER TABLE post_category RENAME TO program_category;"
    addMigration True "ALTER TABLE program_category RENAME COLUMN post TO program;"

-- | Gets the current version of the database.
-- If no version is defined, initialize the
-- version to the latest version and return that.
getDatabaseVersion :: MonadIO m => SqlPersistT m Int
getDatabaseVersion = do
    result <- selectFirst [] []
    case result of
        Just entity -> pure $ schemaVersionVersion $ entityVal entity
        Nothing -> do
            let latestVersion = maximum $ map version migrationList
            setDatabaseVersion latestVersion
            pure latestVersion

-- | Sets the database version number to newVersion
setDatabaseVersion :: MonadIO m => Int -> SqlPersistT m ()
setDatabaseVersion newVersion = do
    result <- selectFirst [] []
    case result of
        Just (Entity key _) -> update key [SchemaVersionVersion =. newVersion]
        Nothing -> insert_ $ SchemaVersion newVersion
