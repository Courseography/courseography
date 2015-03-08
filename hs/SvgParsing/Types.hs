module SvgParsing.Types where

import Database.Tables

-- | A Graph.
data Graph =
    Graph { 
            gId :: Int,
            title :: String
          } deriving Show

-- | A Path.
--data Path =
--    Path {
--           pathId :: String,
--           points :: [Point],
--           pathFill :: String,
--           pathStroke :: String,
--           pathIsRegion :: Bool,
--           source :: String,
--           target :: String
--         } deriving Show

data Style =
    Style {
            transform :: (Double, Double),
            fill :: String,
            stroke :: String
          } deriving Show