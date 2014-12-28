{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}

module Main where
import qualified Data.Text as T
import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8
import Data.String
import Control.Monad    (msum)
import Happstack.Server
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import MakeElements
import MasterTemplate
import GridResponse
import GraphResponse
import AboutResponse
import JsonParser
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class  (liftIO)

import Database.Persist
import Database.Persist.Sqlite

graph :: String
graph = "graph"

grid :: String
grid = "grid"

about :: String
about = "about"

static :: String
static = "static"

staticDir :: String
staticDir = "C:\\Users\\David\\Documents\\courseography"
--staticDir = "/home/cynic/4/courseography"

course :: String
course = "course"

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir grid $ gridResponse,
         dir graph $ graphResponse,
         dir about $ aboutResponse,
         dir static $ serveDirectory EnableBrowsing [] staticDir,
         dir course $ path (\s -> liftIO $ queryCourse s)
       ]

queryCourse :: String -> IO Response
queryCourse course = runSqlite (T.pack ("database/" ++ T.unpack dbStr)) $ do
        --let sql = "SELECT * FROM Lectures WHERE code like '%" ++ course ++ "'"
        --rawQuery sql [] $$ CL.mapM_ (liftIO . print)
        sql :: [Entity Courses] <- selectList [CoursesCode ==. "CSC108H1"] []
        --let c = entityVal $ Prelude.head sql
        return $ toResponseBS (Data.ByteString.Char8.pack "application/json") $ (Data.ByteString.Lazy.Char8.pack $ Prelude.reverse $ Prelude.tail $ Prelude.reverse $ Prelude.tail $ (Prelude.filter (\c -> c /= '\\') $ Data.ByteString.Lazy.Char8.unpack $ Aeson.encode $ (toJsonText $ entityVal $ Prelude.head sql)))
