{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
 OverloadedStrings, TypeFamilies #-}


import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist
import Database.Persist.Sqlite
import Database.JsonParser
import Database.Tables
import WebParsing.ParseAll

main :: IO ()
main = runResourceT $ do
                        liftIO setupDistributionTable
                        liftIO $ print "Distribution table set up"
                        liftIO setupBreadthTable
                        liftIO $ print "breadth table set up"
                        liftIO parseAll

-- | Sets up the Distribution table.
setupDistributionTable :: IO ()
setupDistributionTable = runSqlite dbStr $ do
                                     runMigration migrateAll
                                     insert_ $ Distribution 1 "Humanities"
                                     insert_ $ Distribution 2 "Social Sciences"
                                     insert_ $ Distribution 3 "Sciences"

-- | Sets up the Breadth table.
setupBreadthTable :: IO ()
setupBreadthTable = runSqlite dbStr $ do
                                     runMigration migrateAll
                                     insert_ $ Breadth 1 "Creative and Cultural Representations"
                                     insert_ $ Breadth 2 "Thought, Belief, and Behaviour"
                                     insert_ $ Breadth 3 "Society and Its Institutions"
                                     insert_ $ Breadth 4 "Living Things and Their Environment"
                                     insert_ $ Breadth 5 "The Physical and Mathematical Universes"
                                     insert_ $ Breadth 6 "No Breadth"
