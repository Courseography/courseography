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
import Data.Text
import Data.Aeson
import GHC.Generics
import System.Directory

import           Database.Persist
import           Database.Persist.Sqlite

import Control.Monad
import qualified Data.Conduit.List as CL
import Control.Applicative
import Tables

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
    Tutorial { tut_section :: Text,
               times   :: Text,
               timeStr :: Text
             } deriving (Show)

-- | A Session.
data Session =
    Session { lectures :: [Lecture],
              tutorials  :: [Text]
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

-- | Opens a directory contained in dir, and processes every file in that directory.
processDirectory :: String -> IO ()
processDirectory dir = getDirectoryContents dir >>= \ contents ->
                       let formattedContents = ((Prelude.map ("../../res/courses/" ++) contents))
		                   in filterM doesFileExist formattedContents >>= mapM_ printFile

-- | Opens and reads a files contents, and decodes JSON content into a Course data structure.
printFile :: String -> IO ()
printFile courseFile = do
                         print "Checking"
                         let xf = getJSON courseFile
                         d <- ((eitherDecode <$> xf) :: IO (Either String [Course]))
                         case d of
                           Left err -> print $ courseFile ++ " " ++ err
                           Right course -> do
                                             insertCourse $ Prelude.last course
                                             insertLectures $ Prelude.last course
                                             insertTutorials $ Prelude.last course
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
                                          (getBreadthRequirement      $  breadth course)
                                          (getDistributionRequirement $  distribution course)

-- | Inserts the lectures from course into the Lectures table.
insertLectures :: Course -> IO ()
insertLectures course = do
                          insertSessionLectures (f course) "F" course
                          insertSessionLectures (s course) "S" course

-- | Inserts the lectures from a specified section into the Lectures table.
insertSessionLectures :: Maybe Session -> String -> Course -> IO ()
insertSessionLectures session sessionStr course = case session of
                            Just value -> liftIO $ Prelude.foldl1 (>>) $ Prelude.map ((insertLecture "S") (course)) (lectures value)
                            Nothing    -> print $ "No " ++ sessionStr ++ " lecture section for: " ++ show (name course)

-- | Inserts a lecture into the Lectures table.
insertLecture :: Text -> Course -> Lecture -> IO ()
insertLecture session course lecture = runSqlite dbStr $ do
                                       runMigration migrateAll
                                       insert_ $ Lectures (name course)
                                                          session
                                                          (section lecture)
                                                          (Prelude.map Time (time lecture))
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
insertSessionTutorials :: Maybe Session -> String -> Course -> IO ()
insertSessionTutorials session sessionStr course = case session of
                            Just value -> if Prelude.null (tutorials value)
                                          then print "Cannot find tut"
                                          else liftIO $ Prelude.foldl1 (>>) $ Prelude.map ((insertTutorial "S") (course)) (tutorials value)
                            Nothing    -> print $ "No " ++ sessionStr ++ " tutorial section for: " ++ show (name course)

-- | Inserts a tutorial into the Tutorials table.
insertTutorial :: Text -> Course -> Text -> IO ()
insertTutorial session course tutorial = runSqlite dbStr $ do
                                       runMigration migrateAll
                                       --let tut = parseOnly parseTutorial tutorial
                                       insert_ $ Tutorials (name course)
                                                           session
                                                           tutorial

-- | Gets the corresponding numeric requirement from a breadth requirement description.
-- | 6 indicates a parsing error.
getBreadthRequirement :: Text -> Int
getBreadthRequirement reqString
    |   (isInfixOf "5" reqString) = 5
    |   (isInfixOf "4" reqString) = 4
    |   (isInfixOf "3" reqString) = 3
    |   (isInfixOf "2" reqString) = 2
    |   (isInfixOf "1" reqString) = 1
    |   otherwise = 6

-- | Gets the corresponding numeric requirement from a distribution requirement description.
-- | 6 indicates a parsing error.
getDistributionRequirement :: Text -> Int
getDistributionRequirement reqString
    |   (isInfixOf "This is a Science course" reqString) = 3
    |   (isInfixOf "This is a Social Science course" reqString) = 2
    |   (isInfixOf "This is a Humanities course" reqString) = 1
    |   otherwise = 6

dbStr :: Text
dbStr = "11data34.sqlite3"
