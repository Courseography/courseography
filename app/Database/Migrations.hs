module Database.Migrations
    (migrateDatabase) where

import Control.Monad.Reader (MonadIO)
import Database.Database (getDatabaseVersion)
import Database.Persist.Sql (Migration, SqlPersistT, addMigration, runMigration, runMigrationUnsafe)
import Database.Tables (migrateAll)

data MigrationWrapper = MigrationWrapper {
    version :: Int,
    script :: Migration
}

-- | Migrates the database
migrateDatabase :: MonadIO m => SqlPersistT m ()
migrateDatabase = do
    -- Run unsafe migrations
    currVersion <- getDatabaseVersion
    applyMigrations currVersion migrationList
    -- Run safe migrations
    runMigration migrateAll

-- | Migrates the database by applying only migrations newer than the current version number
applyMigrations :: MonadIO m => Int -> [MigrationWrapper] -> SqlPersistT m ()
applyMigrations currVersion migrations = do
    mapM_ (runMigrationUnsafe . script) $ filter (\migration -> version migration > currVersion) migrations

-- | List of migrations
migrationList :: [MigrationWrapper]
migrationList = [MigrationWrapper {version=2, script=renamePostTables}]

-- | Migration script which renames the Post tables to Program
renamePostTables :: Migration
renamePostTables = do
    addMigration True "ALTER TABLE post RENAME TO program;"
    addMigration True "ALTER TABLE post_category RENAME TO program_category;"
    addMigration True "ALTER TABLE program_category RENAME COLUMN post TO program;"
