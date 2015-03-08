{-# LANGUAGE OverloadedStrings #-}

module Database.SvgDatabase where

import Control.Monad.IO.Class  (liftIO, MonadIO)
import Database.Persist.Sqlite
import Database.Persist
import Control.Monad.Trans.Reader
import Database.Tables
import SvgParsing.Types
import SvgParsing.ParserUtil
import qualified Data.Conduit.List as CL
import Data.Conduit
import Database.JsonParser

insertShapes :: MonadIO m0 => ([Path],[Shape],[Text]) -> ReaderT SqlBackend m0 ()
insertShapes (a,b,c) = foldl (>>) (return ()) $ map insert_ b

insertPaths :: MonadIO m0 => ([Path],[Shape],[Text]) -> ReaderT SqlBackend m0 ()
insertPaths (a,b,c) = foldl (>>) (return ()) $ map insert_ a

insertTexts :: MonadIO m0 => ([Path],[Shape],[Text]) -> ReaderT SqlBackend m0 ()
insertTexts (a,b,c) = foldl (>>) (return ()) $ map insert_ c

--
-- | Prints the database table 'rects'.
printDB :: IO ()
printDB = runSqlite dbStr $ do
              let sql = "SELECT * FROM path"
              rawQuery sql [] $$ CL.mapM_ (liftIO . print)