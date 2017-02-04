{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}

module Database.PostInsertion(
    insertPost, insertPostCategory) where

import qualified Data.Text as T
import Config (databasePath)
import Database.Persist.Sqlite (insert_, runSqlite, runMigration, SqlPersistM)
import Database.Tables

-- | Insert a new post into the database
insertPost :: T.Text -> T.Text -> T.Text -> T.Text -> IO ()
insertPost departmentName postName postCode description =
    runSqlite databasePath $ do
        insert_ $ Post postName departmentName postCode description  :: SqlPersistM ()

-- | Insert a new post category into the database
insertPostCategory :: T.Text -> T.Text -> IO ()
insertPostCategory postCategoryName postCode =
    runSqlite databasePath $ do
        runMigration migrateAll
        insert_ $ PostCategory postCategoryName postCode :: SqlPersistM ()
