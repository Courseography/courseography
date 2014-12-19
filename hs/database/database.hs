{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics
import System.Directory	
import Control.Monad
import Control.Applicative

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
    times [Int] - [[]]
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
    -- dId Int
    description String
    deriving Show
|]

processDirectoryContents :: IO [FilePath] -> t0
processDirectoryContents [] = 0
processDirectoryContents (x:xs) = do
    processFile "../../copy/courses/" ++ x
    processDirectoryContents xs

processDirectory :: String -> Control.Monad.Trans.Reader.ReaderT
                     SqlBackend
                     (Control.Monad.Logger.NoLoggingT
                        (Control.Monad.Trans.Resource.Internal.ResourceT IO))

processDirectory path = processDirectoryContents $ getDirectoryContents path

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        processDirectory $ "../../copy/courses"
        insert $ Distribution "David"
        liftIO $ print "Ian"



getJSON :: String -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

processFile :: String -> IO ()
processFile path = do
    d <- (eitherDecode <$> getJSON path) :: IO (Either String [Course])
    case d of
     Left err -> putStrLn err
     Right ps -> print ps


data Session =
    Session { tutorials :: [Lecture],
              lectures  :: [[Tutorial]]
            }


data Course = 
    Course { breadth     :: String,
             description :: String,
             title       :: String,
             prereqString :: String,
             f           :: Session,
             s           :: Session,
             name        :: String,
             exclusions  :: String,
             manualTutorialEnrol :: Bool,
             distribution :: String,
             prereqs     :: [String]
	   }

data Lecture =
    Lecture { extra :: Int,
              section :: String,
              cap  :: Int,
              time_str :: String,
              time :: [[Int]],
              instructor :: String,
              enrol :: Int,
              wait :: Int
            }

data Tutorial =
    Tutorial { times   :: [[Int]],
               timeStr :: String
             }