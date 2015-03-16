{-# LANGUAGE OverloadedStrings #-}

module Database.SvgDatabase where

import Control.Monad.IO.Class  (liftIO, MonadIO)
import Database.Persist.Sqlite
import Database.Persist
import Control.Monad.Trans.Reader
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Text.Internal as TI
import Database.JsonParser

-- | Performs a query on the database.
queryDatabase :: TI.Text -> IO ()
queryDatabase sql = runSqlite dbStr $ rawQuery sql [] $$ CL.mapM_ (liftIO . print)