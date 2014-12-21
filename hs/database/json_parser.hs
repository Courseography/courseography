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

data Tutorial =
    Tutorial { times   :: [[Int]],
               timeStr :: String
             } deriving (Show)

data Session =
    Session { tutorials :: [Lecture],
              lectures  :: [[Tutorial]]
            } deriving (Show)

data Course = 
    Course { breadth               :: !Text,
             description           :: !Text,
             title               :: !Text,
             prereqString        :: Maybe Text,
             f                   :: Maybe Session,
             s                   :: Maybe Session,
             name                :: !Text,
             exclusions          :: Maybe Text,
             manualTutorialEnrol :: Maybe Bool,
             distribution        :: !Text,
             prereqs             :: Maybe [Text]
	   } deriving (Show, Generic)

main :: IO ()
main = liftIO $ processDirectory $ "../../copy/courses"

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

processDirectory :: String -> IO ()
processDirectory x = getDirectoryContents x >>= \ xd -> 
                     let xy = ((Prelude.map ("../../copy/courses/" ++) xd))
		     in filterM doesFileExist xy >>= mapM_ printFile

printFile :: String -> IO ()
printFile x =  do
                 d <- (eitherDecode <$> (getJSON (x))) :: IO (Either String [Course])
                 case d of
                   Left err -> putStrLn $ x ++ err
                   Right ps -> print ("SUCCESS")

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
