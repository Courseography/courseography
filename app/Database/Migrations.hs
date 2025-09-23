module Database.Migrations
    (renamePostTable) where

import Database.Persist.Sql (Migration, addMigration)

-- | Runs a SQL migration which renames the Post table to Program
renamePostTable :: Migration
renamePostTable = do
    addMigration True "ALTER TABLE Post RENAME TO Program;"
