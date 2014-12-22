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
import           Database.Persist.Postgresql
import           Database.Persist.TH
import Data.Text
import GHC.Generics
import System.Directory	

connStr = "host=localhost dbname=coursedb user=cynic password=**** port=5432"

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
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        insert $ Distribution 1 "Humanities"
        insert $ Distribution 2 "Social Sciences"
        insert $ Distribution 3 "Sciences"
        liftIO $ print "Complete"
