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
import Data.Text
import GHC.Generics
import System.Directory	
import Data.Conduit
import qualified Data.Conduit.List as CL

connStr = "host=localhost dbname=coursedb user=cynic password=eriatarka port=5432"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Courses
    department String
    code Int
    breadth Int
    title String
    description String
    manualTutorialEnrolment Bool
    manualPracticalEnrolment Bool
    prereqs [String]
    exclusions [String]
    distribution Int
    prep String
    deriving Show

Lectures
    department String
    code Int
    session String
    lid String
    times [Int] -- [[]]
    capacity Int
    enrolled Int
    waitlist Int
    extra Int
    location String
    time_str String
    deriving Show

Tutorials
    department String
    cNum Int
    tId String
    times [Int] -- [[]]
    deriving Show

Breadth
    bId Int
    description String
    deriving Show

Distribution
    dId Int
    description String
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
        runMigration migrateAll

        insert $ Distribution 1 "Humanities"
        insert $ Distribution 2 "Social Sciences"
        insert $ Distribution 3 "Sciences"

        insert $ Breadth 1 "Creative and Cultural Representations"
        insert $ Breadth 2 "Thought, Belief, and Behaviour"
        insert $ Breadth 3 "Society and Its Institutions"
        insert $ Breadth 4 "Living Things and Their Environment"
        insert $ Breadth 5 "The Physical and Mathematical Universes"
        let sql = "SELECT * FROM Distribution"
        rawQuery sql [] $$ CL.mapM_ (liftIO . print)
        liftIO $ print "Complete"
