{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}

module Database.PostInsertion(
    insertPost, insertPostCategory) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import Happstack.Server.SimpleHTTP
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Maybe (fromMaybe)
import Config (databasePath)
import Database.Persist.Sqlite (selectFirst, fromSqlKey, toSqlKey, insertMany_, insert_, insert, SqlBackend, (=.), (==.), updateWhere, runSqlite, runMigration)
import Database.Tables
import Data.Aeson

-- | Insert a new post into the database
insertPost :: T.Text -> T.Text -> T.Text -> IO ()
insertPost departmentName postName postCode =
    runSqlite databasePath $ do
        insert_ $ Post postName departmentName postCode

-- | Insert a new post category into the database
insertPostCategory :: T.Text -> T.Text -> IO ()
insertPostCategory postCategoryName postName =
    runSqlite databasePath $ do
        runMigration migrateAll
        insert_ $ PostCategory postCategoryName postName

