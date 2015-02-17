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
           rectId :: String,
           width :: Rational,
           height :: Rational,
           xPos :: Rational,
           yPos :: Rational,
           rectFill :: String,
           rectStroke :: String,
           rectFillOpacity :: String,
           rectIsHybrid :: Bool,
           rectText :: [Text],
           rectInEdges :: [String],
           rectOutEdges :: [String]
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
           pathId :: String,
           points :: [(Rational, Rational)],
           pathFill :: String,
           pathFillOpacity :: String,
           pathStroke :: String,
           pathIsRegion :: Bool,
           source :: String,
           target :: String
         } deriving Show

-- | A Ellipse.
data Ellipse =
    Ellipse { 
              ellipseId :: String,
              ellipseXPos :: Rational,
              ellipseYPos :: Rational,
              ellipseRx :: Rational,
              ellipseRy :: Rational,
              ellipseStroke :: String,
              ellipseText :: [Text],
              ellipseInEdges :: [String],
              ellipseOutEdges :: [String]
            } deriving Show

data Style =
    Style {
            transform :: (Float,Float),
            fill :: String,
            fontSize :: String,
            stroke :: String,
            fillOpacity :: String,
            fontWeight :: String,
            fontFamily :: String
          } deriving Show

class Shape a where
    getX, getY, getWidth, getHeight, getTolerance :: a -> Float
    getId :: a -> String

instance Shape Rect where
    getX a = fromRational $ xPos a
    getY a = fromRational $ yPos a
    getWidth a = fromRational $ width a
    getHeight a = fromRational $ height a
    getId a = rectId a
    getTolerance a = 9

instance Shape Ellipse where
    getX a = fromRational $ ellipseXPos a
    getY a = fromRational $ ellipseYPos a
    getWidth a = 5
    getHeight a = 5
    getId a = ellipseId a
    getTolerance a = 20