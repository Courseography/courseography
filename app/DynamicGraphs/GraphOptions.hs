{-# LANGUAGE OverloadedStrings #-}
module DynamicGraphs.GraphOptions where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))
import qualified Data.Text.Lazy as T

data GraphOptions =
    GraphOptions { taken :: [T.Text],        -- courses to exclude from graph
                   departments :: [T.Text],  -- department prefixes to include
                   excludedDepth :: Int,     -- depth to recurse on courses from excluded departments
                   maxDepth :: Int,          -- total recursive depth to recurse on (depth of the overall graph)
                   courseNumPrefix :: [Int], -- filter based on course number (most useful for filtering based on year)
                   distribution :: [T.Text], -- distribution to include: like "artsci", or "engineering"
                   location :: [T.Text],     -- location of courses to include: like "utsg", or "utsc"
                   includeRaws :: Bool,      -- True to include nodes which are raw values
                   includeGrades :: Bool     -- True to include grade nodes
                } deriving (Show)

data CourseGraphOptions = CourseGraphOptions { courses :: [T.Text], graphOptions :: GraphOptions }
  deriving (Show)

defaultGraphOptions :: GraphOptions
defaultGraphOptions =
    GraphOptions []                    -- taken
                 []                    -- departments
                 0                     -- excludedDepth
                 (-1)                  -- maxDepth
                 []                    -- courseNumPrefix
                 []                    -- distribution
                 []                    -- location
                 True                  -- includeRaws
                 True                  -- includeGrades

instance FromJSON CourseGraphOptions where
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
    incGrades <- o .:? "includeGrades" .!= True
    let options = GraphOptions takenCourses
                               dept
                               excludedCourseDepth
                               maxGraphDepth
                               courseNumPref
                               distrib
                               includedLocation
                               incRaws
                               incGrades
    return $ CourseGraphOptions rootCourses options
