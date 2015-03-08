module SvgParsing.Types where

import Database.Tables

-- | A Graph.
data Graph =
    Graph { 
            gId :: Int,
            title :: String
          } deriving Show

-- | A Text.
data Text =
    Text {
           textXPos :: Double,
           textYPos :: Double,
           textText :: String
         } deriving Show

-- | A Path.
data Path =
    Path { 
           pathId :: String,
           points :: [Point],
           pathFill :: String,
           pathStroke :: String,
           pathIsRegion :: Bool,
           source :: String,
           target :: String
         } deriving Show

data Shape =
    Shape {
        shapeId :: String,
        shapeXPos :: Double,
        shapeYPos :: Double,
        shapeWidth :: Double,
        shapeHeight :: Double,
        shapeFill :: String,
        shapeStroke :: String,
        shapeText :: [Text],
        shapeIsHybrid :: Bool,
        shapeTolerance :: Double
         } deriving Show

data Style =
    Style {
            transform :: (Double, Double),
            fill :: String,
            stroke :: String
          } deriving Show