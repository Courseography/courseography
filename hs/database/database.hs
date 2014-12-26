{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text
import GHC.Generics
import System.Directory	
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Conduit
import qualified Data.Conduit.List as CL
import JsonParser

connStr = "host=localhost dbname=coursedb user=cynic password=*** port=5432"

main :: IO ()
main = runResourceT $ do
                        liftIO $ setupDistributionTable
                        liftIO $ print "Distribution table set up"
                        liftIO $ setupBreadthTable
                        liftIO $ print "breadth table set up"
                        liftIO $ processDirectory $ "../../copy/courses"
                        liftIO $ query


setupDistributionTable :: IO ()
setupDistributionTable = runSqlite dbStr $ do
                                     runMigration migrateAll 
                                     insert_ $ Distribution 1 "Humanities"
                                     insert_ $ Distribution 2 "Social Sciences"
                                     insert_ $ Distribution 3 "Sciences"


setupBreadthTable :: IO ()
setupBreadthTable = runSqlite dbStr $ do
                                     runMigration migrateAll 
                                     insert_ $ Breadth 1 "Creative and Cultural Representations"
                                     insert_ $ Breadth 2 "Thought, Belief, and Behaviour"
                                     insert_ $ Breadth 3 "Society and Its Institutions"
                                     insert_ $ Breadth 4 "Living Things and Their Environment"
                                     insert_ $ Breadth 5 "The Physical and Mathematical Universes"
                                     insert_ $ Breadth 6 "No Breadth"