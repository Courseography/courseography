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
import qualified Data.ByteString.Lazy as B
import Data.Text
import Data.Aeson
import GHC.Generics
import System.Directory	
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal
import Control.Monad.Trans.Reader
import Control.Monad
import Control.Applicative

connStr = "host=localhost dbname=coursedb user=cynic password=**** port=5432"

data Course = 
    Course { breadth               :: !Text,
             description           :: !Text,
             title               :: !Text,
             prereqString        :: !Text,
             f                   :: !Text, --Session,
             s                   :: !Text, --Session,
             name                :: !Text,
             exclusions          :: !Text,
             manualTutorialEnrol :: Bool,
             distribution        :: !Text,
             prereqs             :: ![Text]
	   } deriving (Show, Generic)

fileJ :: FilePath
fileJ = "./file.json"

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

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll
--        processDirectory $ "../../copy/courses"
        insert $ Distribution "David"
        liftIO $ processDirectory $ "../../copy/courses"

instance FromJSON Course where
    parseJSON (Object v) = 
        Course <$> v .: "breadth"
               <*> v .: "description"
               <*> v .: "title"
               <*> v .: "prereqString"
               <*> v .: "F"
               <*> v .: "S"
               <*> v .: "name"
               <*> v .: "exclusions"
               <*> v .: "manualTutorialEnrolment"
               <*> v .: "distribution"
               <*> v .: "prereqs"
    parseJSON _ = mzero

--instance FromJSON Session where
--    parseJSON (Object v) =

--instance FromJSON Lecture
--    parseJSON (Object v) =

--instance FromJSON Tutorial
--    parseJSON (Object v) =

printDirectory :: String -> IO ()
printDirectory x = do 
                       files <- getDirectoryContents x
                       print files

processDirectory :: String -> IO ()
processDirectory x = do
                       files <- getDirectoryContents x
                       printFiles files

printFiles :: [String] -> IO ()
printFiles [] = print "Done"
printFiles (x:xs) = do
                      f <- doesFileExist $ "../../copy/courses/" ++ x
                      if f 
                      then do 
                             d <- (eitherDecode <$> (getJSON ("../../copy/courses/" ++ x))) :: IO (Either String [Course])
                             print d
                             --case d of
                             --  Left err -> putStrLn err
                             --  Right ps -> print (ps)
                      else print "Directory"
                      printFiles xs 

(+++) :: Monad m => m [a] -> m [a] -> m [a]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2

openJSON :: B.ByteString
openJSON = "["

closeJSON :: B.ByteString
closeJSON = "]"

getJSON :: String -> IO B.ByteString
getJSON jsonFile = do
                     a <- (B.readFile jsonFile)
                     let b = B.append openJSON a
                     let c = B.append b closeJSON
		     return c
data Session =
    Session { tutorials :: [Lecture],
              lectures  :: [[Tutorial]]
            } deriving (Show)


data Lecture =
    Lecture { extra :: Int,
              section :: String,
              cap  :: Int,
              time_str :: String,
              time :: [[Int]],
              instructor :: String,
              enrol :: Int,
              wait :: Int
            } deriving (Show)

data Tutorial =
    Tutorial { times   :: [[Int]],
               timeStr :: String
             } deriving (Show)