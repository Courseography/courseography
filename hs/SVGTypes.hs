module SVGTypes where

-- | A Graph.
data Graph =
    Graph { 
            gId :: Int,
            title :: String
          } deriving Show

-- | A Rect.
data Rect =
    Rect {
           width :: Rational,
           height :: Rational,
           xPos :: Rational,
           yPos :: Rational,
           style :: String
         } deriving Show

-- | A Text.
data Text =
    Text {
           textXPos :: Rational,
           textYPos :: Rational,
           textText :: String,
           textStyle :: String
         } deriving Show

-- | A Path.
data Path =
    Path { 
           points :: [(Rational, Rational)],
           pathStyle :: String
         }