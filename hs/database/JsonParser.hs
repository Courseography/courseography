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

import Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import System.Directory

import Database.Persist
import Database.Persist.Sqlite

import Control.Monad
import qualified Data.Conduit.List as CL
import Control.Applicative
import Tables

courseDirectory :: String
courseDirectory = "../../res/courses2/"

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
    Tutorial { times       :: [[Int]],
               timeStr     :: T.Text
             } deriving Show

-- | A Session.
data Session =
    Session { lectures   :: [Lecture],
              tutorials  :: [Tutorial]
            } deriving Show

-- | A Course.
data Course =
    Course { breadth               :: !T.Text,
             description           :: !T.Text,
             title                 :: !T.Text,
             prereqString          :: Maybe T.Text,
             f                     :: Maybe Session,
             s                     :: Maybe Session,
             y                     :: Maybe Session,
             name                  :: !T.Text,
             exclusions            :: Maybe T.Text,
             manualTutorialEnrol   :: Maybe Bool,
             distribution          :: !T.Text,
             prereqs               :: Maybe [T.Text]
           } deriving Show

instance FromJSON Course where
    parseJSON (Object v) =
        Course <$> v .:  "breadth"
               <*> v .:  "description"
               <*> v .:  "title"
               <*> v .:  "prereqString"
               <*> v .:? "F"
               <*> v .:? "S"
               <*> v .:? "Y"
               <*> v .:  "name"
               <*> v .:  "exclusions"
               <*> v .:? "manualTutorialEnrolment"
               <*> v .:  "distribution"
               <*> v .:? "prereqs"
    parseJSON _ = mzero

instance ToJSON Course where
  toJSON (Course breadth description title prereqString f s y name exclusions manualTutorialEnrol distribution prereqs) 
          = object ["breadth" .= breadth,
                    "description" .= description,
                    "title" .= title,
                    "prereqString" .= prereqString,
                    "F" .= f,
                    "S" .= s,
                    "Y" .= y,
                    "name" .= name,
                    "exclusions" .= exclusions,
                    "manualTutorialEnrolment" .= manualTutorialEnrol,
                    "distribution" .= distribution,
                    "prereqs" .= prereqs
                              ]

instance FromJSON Session where
    parseJSON (Object v) =
        Session <$> v .: "lectures"
                <*> v .: "tutorials"
    parseJSON _ = mzero

instance ToJSON Session where
  toJSON (Session lectures tutorials) 
          = object ["lectures" .= lectures,
                    "tutorials" .= tutorials
                   ]

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

instance ToJSON Lecture where
  toJSON (Lecture extra section cap time_str time instructor enrol wait) 
          = object ["extra" .= extra,
                    "section" .= section,
                    "cap" .= cap,
                    "time_str" .= time_str,
                    "time" .= time,
                    "instructor" .= instructor,
                    "enrol" .= enrol,
                    "wait" .= wait
                   ]

instance FromJSON Tutorial where
    parseJSON (Array v)
        | V.length v == 2 = do
            times <- parseJSON $ v V.! 0
            timeStr <- parseJSON $ v V.! 1
            return $ Tutorial times timeStr
        | otherwise = mzero
    parseJSON _ = mzero

instance ToJSON Tutorial where
  toJSON (Tutorial times timeStr) 
          = Array (V.fromList (map toJSON times) V.++ (V.singleton $ toJSON timeStr))

-- | Opens a directory contained in dir, and processes every file in that directory.
processDirectory :: IO ()
processDirectory = getDirectoryContents courseDirectory >>= \contents ->
                    let formattedContents = (map (courseDirectory ++) contents)
                    in filterM doesFileExist formattedContents >>= mapM_ printFile

-- | Opens and reads a files contents, and decodes JSON content into a Course data structure.
printFile :: String -> IO ()
printFile courseFile = do
                         d <- ((eitherDecode <$> getJSON courseFile) :: IO (Either String Course))
                         case d of
                           Left err -> print $ courseFile ++ " " ++ err
                           Right course -> do
                                             insertCourse $ course
                                             insertLectures $ course
                                             insertTutorials $ course

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
                                          (breadth course) 
                                          (distribution course)
                                          (prereqString course)

-- | Inserts the lectures from course into the Lectures table.
insertLectures :: Course -> IO ()
insertLectures course = insertSessionLectures (f course) "F" course >>
                        insertSessionLectures (s course) "S" course >>
                        insertSessionLectures (y course) "Y" course

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
                                                          (instructor lecture)
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
                          insertSessionTutorials (s course) "S" course >>
                          insertSessionTutorials (y course) "Y" course

-- | Inserts the tutorials from a specified section into the Tutorials table.
insertSessionTutorials :: Maybe Session -> T.Text -> Course -> IO ()
insertSessionTutorials session sessionStr course = case session of
                            Just value -> if null (tutorials value)
                                          then print $ "Cannot find tutorial for" ++ show (name course)
                                          else liftIO $ mapM_ ((insertTutorial sessionStr) course) (tutorials value)
                            Nothing    -> print $ "No " ++ (T.unpack sessionStr) ++ " tutorial section for: " ++ show (name course)

-- | Inserts a tutorial into the Tutorials table.
insertTutorial :: T.Text -> Course -> Tutorial -> IO ()
insertTutorial session course tutorial = runSqlite dbStr $ do
                                       runMigration migrateAll
                                       insert_ $ Tutorials (name course)
                                                           session
                                                           (map Time  (times tutorial))
                                                           (timeStr tutorial)

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
