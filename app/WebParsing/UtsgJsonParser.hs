{-# LANGUAGE OverloadedStrings #-}

{-
module WebParsing.UtsgJsonParser
     (getJSON) where
-}

import Data.Aeson
import Data.List
import Database.Tables
import WebParsing.PrerequisiteParsing
import Database.Persist.Sqlite (runSqlite, insertMany_)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import Config (databasePath)
import Network.HTTP.Conduit (simpleHttp)

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
        codes                     = map (\(Just c) -> c) codes'
    let courseTitles              = lookupField "courseTitle"              allMetadata
        courseDescriptions        = lookupField "courseDescription"        allMetadata
        prereqs                   = fmap        parsePrerequisites         prereqStrings
        exclusions                = lookupField "exclusion"                allMetadata
        breadths                  = lookupField "breadthCategories"        allMetadata
        distributions             = lookupField "distributionCategories"   allMetadata
        prereqStrings             = lookupField "prerequisite"             allMetadata
        coreqs                    = lookupField "corequisite"              allMetadata
    let manualTutorialEnrolments  = repeat      $ Just False
        manualPracticalEnrolments = repeat      $ Just False
    let videoUrls                 = repeat      ([] :: [T.Text])

    let lst7 = zipWith7 (\a b c d e f g -> a:b:c:d:e:f:g:[])
                         courseTitles
                         courseDescriptions
                         prereqs
                         exclusions
                         breadths
                         distributions
                         prereqStrings

    print ("inserting " ++ (show $ length lst7) ++ " courses into database")

    runSqlite databasePath $ insertMany_ $ zipWith6 (\c lst corqs mTutEnrl mPratEnrl vUrl ->
                                                    Courses c (lst !! 0) (lst !! 1) mTutEnrl mPratEnrl (lst !! 2)
                                                            (lst !! 3) (lst !! 4) (lst !! 5) (lst !! 6) corqs vUrl)
                                                    codes
                                                    lst7
                                                    coreqs
                                                    manualTutorialEnrolments
                                                    manualPracticalEnrolments
                                                    videoUrls

    print ("All Courses have been successfully inserted")

