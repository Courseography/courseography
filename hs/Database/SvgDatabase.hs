{-# LANGUAGE OverloadedStrings #-}

module Database.SvgDatabase where

import Control.Monad.IO.Class  (liftIO, MonadIO)
import Database.Persist.Sqlite
import Database.Persist
import Control.Monad.Trans.Reader
import qualified Data.Conduit.List as CL
import Data.Conduit
import Database.JsonParser

-- | Prints the database table 'rects'.
printDB :: IO ()
printDB = runSqlite dbStr $ do
              let sql = "SELECT count(fill) FROM path"
              rawQuery sql [] $$ CL.mapM_ (liftIO . print)