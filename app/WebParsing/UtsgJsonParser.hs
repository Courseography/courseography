{-# LANGUAGE OverloadedStrings #-}

{-
module WebParsing.UtsgJsonParser
     (getJSON) where
-}

import qualified Data.HashMap.Lazy as M
import Network.HTTP.Conduit (simpleHttp)
import WebParsing.PrerequisiteParsing
import Data.Aeson

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
        return $ fmap (\(Just (String v)) -> Just v) searchResults

{- Retrieve all Course Table metadata -}
getAllCourses :: IO ()
getAllCourses = do
    allMetadata <- getMetadata getJSON
    let codes                     = lookupField "code"                     allMetadata
        courseTitles              = lookupField "courseTitle"              allMetadata
        courseDescriptions        = lookupField "courseDescription"        allMetadata
        manualTutorialEnrolments  = lookupField "manualTutorialEnrolment"  allMetadata
        manualPracticalEnrolments = lookupField "manualPracticalEnrolment" allMetadata
        prereqs                   = fmap        parsePrerequisites         prereqStrings
        exclusions                = lookupField "exclusion"                allMetadata
        breadths                  = lookupField "breadthCategories"        allMetadata
        distributions             = lookupField "distributionCategories"   allMetadata
        prereqStrings             = lookupField "prerequisite"             allMetadata
        coreqs                    = lookupField "corequisite"              allMetadata
        videoUrls                 = repeat      []
    print prereqs
