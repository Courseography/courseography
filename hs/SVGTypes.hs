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
           rectFill :: String,
           rectStroke :: String,
           rectFillOpacity :: String,
           rectIsHybrid :: Bool
         } deriving Show

-- | A Text.
data Text =
    Text {
           textXPos :: Rational,
           textYPos :: Rational,
           textText :: String,
           textFontSize :: String,
           textFontWeight :: String,
           textFontFamily :: String
         } deriving Show

-- | A Path.
data Path =
    Path { 
           points :: [(Rational, Rational)],
           pathFill :: String,
           pathFillOpacity :: String,
           pathStroke :: String,
           pathIsRegion :: Bool
         }

-- | A Ellipse.
data Ellipse =
    Ellipse { 
              ellipseXPos :: Rational,
              ellipseYPos :: Rational,
              ellipseRx :: Rational,
              ellipseRy :: Rational,
              ellipseStroke :: String
         }

data Style =
    Style {
            transform :: (Float,Float),
            fill :: String,
            fontSize :: String,
            stroke :: String,
            fillOpacity :: String,
            fontWeight :: String,
            fontFamily :: String
    }