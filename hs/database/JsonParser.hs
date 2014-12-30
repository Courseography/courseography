{-# LANGUAGE EmptyDataDecls, 
             FlexibleContexts,
             GADTs,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             OverloadedStrings,
             DeriveGeneric,
             QuasiQuotes,
             TemplateHaskell,            
             TypeFamilies #-}

module JsonParser where

import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import System.Directory

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import Control.Monad
import qualified Data.Conduit.List as CL
import Control.Applicative

dbStr :: T.Text
dbStr = "file1.sqlite3"

courseDirectory :: String
courseDirectory = "../../res/courses/"

data Time = Time { timeField :: [Int] } deriving (Show, Read, Eq)
derivePersistField "Time"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Courses json
    --department String
    code T.Text
    title T.Text
    description T.Text
    manualTutorialEnrolment Bool Maybe
    --manualPracticalEnrolment Bool
    prereqs T.Text Maybe
    exclusions T.Text Maybe
    breadth Int
    distribution Int
    --prep T.Text
    deriving Show

Lectures
    --department String
    code T.Text
    session T.Text
    section T.Text
    times [Time]
    capacity Int
    enrolled Int
    waitlist Int
    extra Int
    -- location Text -- Location does not exist in JSON files.
    time_str T.Text
    deriving Show

Tutorials
    --department String
    code T.Text
    session T.Text
    tutorial T.Text
    --times [Time]
    --timeStr Text
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
              section    :: T.Text,
              cap        :: Int,
              time_str   :: T.Text,
              time       :: [[Int]],
              instructor :: T.Text,
              enrol      :: Maybe Int,
              wait       :: Maybe Int
            } deriving Show

-- | A Tutorial.
data Tutorial =
    Tutorial { tut_section :: T.Text,
               times   :: T.Text,
               timeStr :: T.Text
             } deriving Show

-- | A Session.
data Session =
    Session { lectures :: [Lecture],
              tutorials  :: [T.Text]
            } deriving Show

-- | A Course.
data Course =
    Course { breadth               :: !T.Text,
             description           :: !T.Text,
             title                 :: !T.Text,
             prereqString          :: Maybe T.Text,
             f                     :: Maybe Session,
             s                     :: Maybe Session,
             name                  :: !T.Text,
             exclusions            :: Maybe T.Text,
             manualTutorialEnrol   :: Maybe Bool,
             distribution          :: !T.Text,
             prereqs               :: Maybe [T.Text]
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

-- | Opens a directory contained in dir, and processes every file in that directory.
processDirectory :: IO ()
processDirectory = getDirectoryContents courseDirectory >>= \contents ->
                    let formattedContents = (map (courseDirectory ++) contents)
		                in filterM doesFileExist formattedContents >>= mapM_ printFile

-- | Opens and reads a files contents, and decodes JSON content into a Course data structure.
printFile :: String -> IO ()
printFile courseFile = do
                         print "Checking"
                         let xf = getJSON courseFile
                         d <- ((eitherDecode <$> xf) :: IO (Either String Course))
                         case d of
                           Left err -> print $ courseFile ++ " " ++ err
                           Right course -> do
                                             insertCourse $ course
                                             insertLectures $ course
                                             insertTutorials $ course
                                             print $ "Inserted " ++ courseFile

-- | Opens and reads the file contained in `jsonFile`. File contents are returned, surrounded by
-- | square brackets.
getJSON :: String -> IO B.ByteString
getJSON jsonFile = (B.readFile jsonFile)

-- | Inserts course into the Courses table.
insertCourse :: Course -> IO ()
insertCourse course = runSqlite dbStr $ do
                        runMigration migrateAll
                        insert_ $ Courses (name course)
                                          (title course)
                                          (description course)
                                          (manualTutorialEnrol course)
                                          (prereqString course)
                                          (exclusions course)
                                          (getBreadthRequirement $ breadth course)
                                          (getDistributionRequirement $ distribution course)

-- | Inserts the lectures from course into the Lectures table.
insertLectures :: Course -> IO ()
insertLectures course = insertSessionLectures (f course) "F" course >>
                        insertSessionLectures (s course) "S" course

-- | Inserts the lectures from a specified section into the Lectures table.
insertSessionLectures :: Maybe Session -> T.Text -> Course -> IO ()
insertSessionLectures session sessionStr course = case session of
                            Just value -> liftIO $ mapM_ ((insertLecture sessionStr) course) (lectures value)
                            Nothing    -> print $ "No " ++ (T.unpack sessionStr) ++ " lecture section for: " ++ show (name course)

-- | Inserts a lecture into the Lectures table.
insertLecture :: T.Text -> Course -> Lecture -> IO ()
insertLecture session course lecture = runSqlite dbStr $ do
                                       runMigration migrateAll
                                       insert_ $ Lectures (name course)
                                                          session
                                                          (section lecture)
                                                          (map Time (time lecture))
                                                          (cap lecture)
                                                          (case enrol lecture of
                                                                Just value -> value
                                                                Nothing    -> 0)
                                                          (case wait lecture of
                                                                Just value -> value
                                                                Nothing    -> 0)
                                                          (extra lecture)
                                                          (time_str lecture)

-- | Inserts the tutorials from course into the Tutorials table.
insertTutorials :: Course -> IO ()
insertTutorials course =  insertSessionTutorials (f course) "F" course >>
                          insertSessionTutorials (s course) "S" course

-- | Inserts the tutorials from a specified section into the Tutorials table.
insertSessionTutorials :: Maybe Session -> T.Text -> Course -> IO ()
insertSessionTutorials session sessionStr course = case session of
                            Just value -> if null (tutorials value)
                                          then print "Cannot find tut"
                                          else liftIO $ mapM_ ((insertTutorial sessionStr) course) (tutorials value)
                            Nothing    -> print $ "No " ++ (T.unpack sessionStr) ++ " tutorial section for: " ++ show (name course)

-- | Inserts a tutorial into the Tutorials table.
insertTutorial :: T.Text -> Course -> T.Text -> IO ()
insertTutorial session course tutorial = runSqlite dbStr $ do
                                       runMigration migrateAll
                                       --let tut = parseOnly parseTutorial tutorial
                                       insert_ $ Tutorials (name course)
                                                           session
                                                           tutorial


-- | Gets the corresponding numeric requirement from a breadth requirement description.
-- | 6 indicates a parsing error.
getBreadthRequirement :: T.Text -> Int
getBreadthRequirement reqString
    |   (T.isInfixOf "5" reqString) = 5
    |   (T.isInfixOf "4" reqString) = 4
    |   (T.isInfixOf "3" reqString) = 3
    |   (T.isInfixOf "2" reqString) = 2
    |   (T.isInfixOf "1" reqString) = 1
    |   otherwise = 6

-- | Gets the corresponding numeric requirement from a distribution requirement description.
-- | 6 indicates a parsing error.
getDistributionRequirement :: T.Text -> Int
getDistributionRequirement reqString
    |   (T.isInfixOf "This is a Science course" reqString) = 3
    |   (T.isInfixOf "This is a Social Science course" reqString) = 2
    |   (T.isInfixOf "This is a Humanities course" reqString) = 1
    |   otherwise = 6
