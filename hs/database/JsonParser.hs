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
--import Yesod
import Data.Text
import Data.Aeson
import GHC.Generics
import System.Directory	
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource.Internal
import Control.Monad.Trans.Reader
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class
import Control.Applicative

data Time = Time { timeField :: [Int] } deriving (Show, Read, Eq)
derivePersistField "Time"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Courses
    --department String
    code Text
    title Text  
    description Text
    manualTutorialEnrolment Bool Maybe
    --manualPracticalEnrolment Bool
    prereqs Text Maybe
    exclusions Text Maybe 
    breadth Int
    distribution Int
    --prep Text
    deriving Show

Lectures
    --department String
    code Text
    session Text
    section Text
    times [Time]
    --capacity Int
    --enrolled Int
    --waitlist Int
    extra Int
    -- location Text -- Location does not exist in JSON files.
    time_str Text
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
              section    :: Text,
              cap        :: Int,
              time_str   :: Text,
              time       :: [[Int]],
              instructor :: Text,
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
    Session { lectures :: [Lecture],
              tutorials  :: [[Tutorial]]
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
        Course <$> v .:  "breadth"
               <*> v .:  "description"
               <*> v .:  "title"
               <*> v .:  "prereqString"
               <*> v .:? "F"
               <*> v .:? "S"
               <*> v .:  "name"
               <*> v .:  "exclusions"
               <*> v .:? "manualTutorialEnrolment"
               <*> v .:  "distribution"
               <*> v .:? "prereqs"
    parseJSON _ = mzero

instance FromJSON Session where
    parseJSON (Object v) =
        Session <$> v .: "lectures"
                <*> v .: "tutorials"
    parseJSON _ = mzero    

instance FromJSON Lecture where
    parseJSON (Object v) =
        Lecture <$> v .:  "extra"
                <*> v .:  "section"
                <*> v .:  "cap"
                <*> v .:  "time_str"
                <*> v .:  "time"
                <*> v .:  "instructor"
                <*> v .:? "enrol"
                <*> v .:? "wait"
    parseJSON _ = mzero

instance FromJSON Tutorial where
    parseJSON (Object v) =
        Tutorial <$> v .: "times"
                 <*> v .: "timeStr"
    parseJSON _ = mzero

--instance FromJSON Time where
--    parseJSON (Object v) =
--        Time <$> v .: "timeField"
--    parseJSON _ = mzero

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
                           Left err -> print $ courseFile ++ " " ++ err
                           Right course -> do 
                                             insertCourse $ Prelude.last course
                                             insertLectures $ Prelude.last course
                                             print $ "Inserted " ++ courseFile

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

insertCourse :: Course -> IO ()
insertCourse course = runSqlite dbStr $ do
                        runMigration migrateAll 
                        insert_ $ Courses (name course) 
                                          (title course)
                                          (description course)
                                          (manualTutorialEnrol course)
                                          (prereqString course)
                                          (exclusions course)
                                          (getRequirement $  breadth course)
                                          (getRequirement $  distribution course)

insertLectures :: Course -> IO ()
insertLectures course = do
                          case (f course) of
                            Just value -> liftIO $ Prelude.foldl1 (>>) $ Prelude.map ((insertLecture "F") (course)) (lectures value)
                            Nothing    -> print "Incomplete"
                                             

insertLecture :: Text -> Course -> Lecture -> IO ()
insertLecture session course lecture = runSqlite dbStr $ do
                                       runMigration migrateAll 
                                       insert_ $ Lectures (name course)
                                                          session
                                                          (section lecture)
                                                          (Prelude.map Time (time lecture))
                                                          (extra lecture)
                                                          (time_str lecture)

getRequirement :: Text -> Int
getRequirement reqString
    |   (isInfixOf "5" reqString) = 5
    |   (isInfixOf "4" reqString) = 4
    |   (isInfixOf "3" reqString) = 3
    |   (isInfixOf "2" reqString) = 2
    |   (isInfixOf "1" reqString) = 1 
    |   (isInfixOf "This is a Science course" reqString) = 3 
    |   (isInfixOf "This is a Social Science course" reqString) = 2
    |   (isInfixOf "This is a Humanities course" reqString) = 1 
    | otherwise = 6 

query :: IO ()
query = runSqlite dbStr $ do
        let sql = "SELECT * FROM Lectures WHERE code like '%CSC%'"
        rawQuery sql [] $$ CL.mapM_ (liftIO . print)

dbStr :: Text
dbStr = "data23.sqlite3"