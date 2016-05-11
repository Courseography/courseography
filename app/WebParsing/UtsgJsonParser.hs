{-# LANGUAGE OverloadedStrings #-}

{-
module WebParsing.UtsgJsonParser
     (getJSON) where
-}

import Data.Aeson
import Data.List
import Database.Tables
import WebParsing.PrerequisiteParsing
import Database.Persist.Sqlite
import qualified Data.HashMap.Lazy as M
import Config (databasePath)
import Network.HTTP.Conduit (simpleHttp)
import Control.Monad (forM_)
import Database.CourseInsertion (insertCourse)

{- URL to UofT courses (stored as JSON string) -}
jsonURL :: String
jsonURL = "https://timetable.iit.artsci.utoronto.ca/api/courses?org=&code=csc108"

{- Decode JSON string to JSON object -}
getJSON :: IO (Maybe Object)
getJSON = do
    resp <- simpleHttp jsonURL
    return $ (decode resp :: Maybe Object)

{- Extract course metadata (values) from JSON object -}
getMetadata :: IO (Maybe (M.HashMap k v)) -> IO [v]
getMetadata = fmap (\(Just v) -> M.elems v)

{- Lookup field (by name) within metadata and then "expose" results. -}
-- lookupField :: Functor f => Data.Text.Internal.Text -> f Value -> f (Maybe Data.Text.Internal.Text)
lookupField fieldName = do
        searchResults <- fmap (\(Object v) -> M.lookup fieldName v)
        -- [Maybe (Value a)] -> [Maybe a]
        return $ fmap (\(Just (String v)) -> Just v) searchResults

{- Retrieve all Course Table metadata -}
getAllCourses :: IO ()
getAllCourses = do
    print ("parsing JSON data from: " ++ jsonURL)
    allMetadata <- getMetadata getJSON
    let codes'                    = lookupField "code"                     allMetadata
        codes                     = fmap (\(Just v) -> v) codes'
        courseTitles              = lookupField "courseTitle"              allMetadata
        courseDescriptions        = lookupField "courseDescription"        allMetadata
        manualTutorialEnrolments  = repeat      $ Just "0"
        manualPracticalEnrolments = repeat      $ Just "0"
        prereqs                   = fmap        parsePrerequisites         prereqStrings
        exclusions                = lookupField "exclusion"                allMetadata
        breadths                  = lookupField "breadthCategories"        allMetadata
        distributions             = lookupField "distributionCategories"   allMetadata
        prereqStrings             = lookupField "prerequisite"             allMetadata
        coreqs                    = lookupField "corequisite"              allMetadata
        videoUrls                 = repeat      $ Just "[]"

    let lst12 = zipWith6 (\a b c d e lst -> a:b:c:d:e:lst)
                         codes
                         courseTitles
                         courseDescriptions
                         manualTutorialEnrolments
                         manualPracticalEnrolments
                         lst7
        lst7  = zipWith7 (\a b c d e f g -> a:b:c:d:e:f:g:[])
                         prereqs
                         exclusions
                         breadths
                         distributions
                         prereqStrings
                         coreqs
                         videoUrls

    print ("inserting " ++ (show $ length lst12) ++ " courses into database")
    {-runSqlite databasePath $ do
        runMigration migrateAll
        -- forM_ -}
    print (lst12)
    print ("All Courses have been successfully inserted")

