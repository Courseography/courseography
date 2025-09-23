module Database.Migrations
    (migrateDatabase) where

import Control.Monad.Reader (MonadIO, ReaderT)
import Database.Persist.Sql (Migration, SqlBackend, addMigration, runMigration, runMigrationUnsafe)
import Database.Tables (migrateAll)

-- | Runs a migration which renames the Post tables to Program
renamePostTables :: Migration
renamePostTables = do
    addMigration True "ALTER TABLE post RENAME TO program;"
    addMigration True "ALTER TABLE post_category RENAME TO program_category;"
    addMigration True "ALTER TABLE program_category RENAME COLUMN post TO program;"

-- | Migrates the database
migrateDatabase :: MonadIO m => ReaderT SqlBackend m ()
migrateDatabase = do
    -- Run unsafe migrations
    runMigrationUnsafe renamePostTables
    -- Run safe migrations
    runMigration migrateAll
