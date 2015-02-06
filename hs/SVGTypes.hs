module SVGTypes where

data Graph =
    Graph { 
            gId :: Int,
            title :: String
          } deriving Show

data Rect =
    Rect {
           width :: Rational,
           height :: Rational,
           xPos :: Rational,
           yPos :: Rational,
           style :: String
         } deriving Show

data Text =
    Text {
           textXPos :: Rational,
           textYPos :: Rational,
           textText :: String,
           textStyle :: String
         } deriving Show


data Path =
    Path { 
           points :: [(Rational, Rational)],
           pathStyle :: String
         }