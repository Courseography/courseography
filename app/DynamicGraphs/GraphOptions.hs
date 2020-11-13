module DynamicGraphs.GraphOptions where

import Data.Aeson ((.:?), (.!=), FromJSON(parseJSON), withObject)
import qualified Data.Text.Lazy as T

data GraphOptions =
    GraphOptions { courses :: [T.Text],     --courses to create a graph for
                  taken :: [T.Text],        -- courses to exclude from graph
                  departments :: [T.Text],  -- department prefixes to include
                  excludedDepth :: Int,     -- depth to recurse on courses from excluded departments
                  maxDepth :: Int,          -- total recursive depth to recurse on (depth of the overall graph)
                  courseNumPrefix :: [Int], -- filter based on course number (most useful for filtering based on year)
                  distribution :: [T.Text], -- distribution to include: like "artsci", or "engineering"
                  location :: [T.Text],     -- location of courses to include: like "st_george", or "scarborough"
                  includeRaws :: Bool,      -- True to include nodes which are raw values
                  includeGrades :: Bool     -- True to include grade nodes
                } deriving (Show)

instance FromJSON GraphOptions where
  parseJSON = withObject "Expected Object for GraphOptions" $ \o -> do
    rootCourses <- o .:? "courses" .!= []
    takenCourses <- o .:? "taken" .!= []
    dept <- o .:? "departments" .!= []
    excludedCourseDepth <- o .:? "excludedDepth" .!= 0
    maxGraphDepth <- o .:? "maxDepth" .!= (-1)
    courseNumPref <- o .:? "courseNumPrefix" .!= []
    distrib <- o .:? "distribution" .!= []
    includedLocation <- o .:? "location" .!= []
    incRaws <- o .:? "includeRaws" .!= True
    incGrades <- o .:? "includeRaws" .!= True
    return $ GraphOptions rootCourses
                          takenCourses
                          dept
                          excludedCourseDepth
                          maxGraphDepth
                          courseNumPref
                          distrib
                          includedLocation
                          incRaws
                          incGrades
