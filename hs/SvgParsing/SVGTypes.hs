module SvgParsing.SVGTypes where

-- | A Graph.
data Graph =
    Graph { 
            gId :: Int,
            title :: String
          } deriving Show

-- | A Text.
data Text =
    Text {
           textXPos :: Rational,
           textYPos :: Rational,
           textText :: String
         } deriving Show

-- | A Path.
data Path =
    Path { 
           pathId :: String,
           points :: [(Rational, Rational)],
           pathFill :: String,
           pathStroke :: String,
           pathIsRegion :: Bool,
           source :: String,
           target :: String
         } deriving Show

data Shape =
    Shape {
        shapeId :: String,
        shapeXPos :: Rational,
        shapeYPos :: Rational,
        shapeWidth :: Rational,
        shapeHeight :: Rational,
        shapeFill :: String,
        shapeStroke :: String,
        shapeText :: [Text],
        shapeIsHybrid :: Bool,
        shapeTolerance :: Float
         } deriving Show

data Style =
    Style {
            transform :: (Float,Float),
            fill :: String,
            stroke :: String
          } deriving Show