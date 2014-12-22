{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module JsonParser where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import qualified Data.ByteString.Lazy as B
import Data.Text
import Data.Aeson
import GHC.Generics
import System.Directory	
import           Database.Persist
import           Database.Persist.TH
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal
import Control.Monad.Trans.Reader
import Control.Monad
import Control.Applicative

--main :: IO ()
--main = liftIO $ processDirectory $ "../../copy/courses"

data Time = Time { timeField :: [Int] } deriving (Show, Read, Eq)
derivePersistField "Time"

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
    times [Time]
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
    times [Time]
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

-- | A Lecture.
data Lecture =
    Lecture { extra      :: Int,
              section    :: String,
              cap        :: Int,
              time_str   :: String,
              time       :: [[Int]],
              instructor :: String,
              enrol      :: Maybe Int,
              wait       :: Maybe Int
            } deriving (Show)

-- | A Tutorial.
data Tutorial =
    Tutorial { times   :: [[Int]],
               timeStr :: String
             } deriving (Show)

-- | A Session.
data Session =
    Session { tutorials :: [Lecture],
              lectures  :: [[Tutorial]]
            } deriving (Show)

-- | A Course.
data Course = 
    Course { breadth               :: !Text,
             description           :: !Text,
             title                 :: !Text,
             prereqString          :: Maybe Text,
             f                     :: Maybe Session,
             s                     :: Maybe Session,
             name                  :: !Text,
             exclusions            :: Maybe Text,
             manualTutorialEnrol   :: Maybe Bool,
             distribution          :: !Text,
             prereqs               :: Maybe [Text]
	   } deriving (Show, Generic)

instance FromJSON Course where
    parseJSON (Object v) = 
        Course <$> v .: "breadth"
               <*> v .: "description"
               <*> v .: "title"
               <*> v .: "prereqString"
               <*> v .:? "F"
               <*> v .:? "S"
               <*> v .: "name"
               <*> v .: "exclusions"
               <*> v .:? "manualTutorialEnrolment"
               <*> v .: "distribution"
               <*> v .:? "prereqs"
    parseJSON _ = mzero

instance FromJSON Session where
    parseJSON (Object v) =
        Session <$> v .: "lectures"
                <*> v .: "tutorials"
    parseJSON _ = mzero    

instance FromJSON Lecture where
    parseJSON (Object v) =
        Lecture <$> v .: "extra"
                <*> v .: "section"
                <*> v .: "cap"
                <*> v .: "time_str"
                <*> v .: "time"
                <*> v .: "instructor"
                <*> v .:? "enrol"
                <*> v .:? "wait"
    parseJSON _ = mzero

instance FromJSON Tutorial where
    parseJSON (Object v) =
        Tutorial <$> v .: "times"
                 <*> v .: "timeStr"
    parseJSON _ = mzero

-- | Opens a directory contained in dir, and processes every file in that directory.
processDirectory :: String -> IO ()
processDirectory dir = getDirectoryContents dir >>= \ contents -> 
                       let formattedContents = ((Prelude.map ("../../copy/courses/" ++) contents))
		       in filterM doesFileExist formattedContents >>= mapM_ printFile

-- | Opens and reads a files contents, and decodes JSON content into a Course data structure.
printFile :: String -> IO ()
printFile courseFile = do
                       d <- ((eitherDecode <$> getJSON courseFile) :: IO (Either String [Course]))
                       case d of
                         Left err -> putStrLn $ courseFile ++ err
                         Right ps -> print (title $ Prelude.last ps)
                       print "I did it"

-- | An opening square bracket.
openJSON :: B.ByteString
openJSON = "["

-- | A closing square bracket.
closeJSON :: B.ByteString
closeJSON = "]"

-- | Opens and reads the file contained in `jsonFile`. File contents are returned, surrounded by
-- | square brackets.
getJSON :: String -> IO B.ByteString
getJSON jsonFile = (B.readFile jsonFile) >>= \ a -> return $ B.append (B.append openJSON a) closeJSON
