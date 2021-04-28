{-|
    Module      : Css.Constants
    Description : Defines the constants for the other CSS modules.
-}
module Css.Constants
    (-- * Colors
     theoryDark,
     coreDark,
     seDark,
     systemsDark,
     graphicsDark,
     dbwebDark,
     numDark,
     aiDark,
     hciDark,
     mathDark,
     introDark,
     titleColour,
     lightGrey,
     -- * Graph Styles
     nodeFontSize,
     hybridFontSize,
     boolFontSize,
     regionFontSize
     ) where

import Prelude hiding ((**))
import Data.Text as T

{- Colors -}

-- |Defines the color of a grayish blue.
theoryDark :: T.Text
theoryDark = "#B1C8D1"

-- |Defines the color of a light gray.
coreDark :: T.Text
coreDark = "#C9C9C9"

-- |Defines the color of a soft red.
seDark :: T.Text
seDark = "#E68080"

-- |Defines the color of a light violet.
systemsDark :: T.Text
systemsDark = "#C285FF"

-- |Defines the color of a mostly desaturated dark lime green.
graphicsDark :: T.Text
graphicsDark = "#66A366"

-- |Defines the color of a strong pink.
dbwebDark :: T.Text
dbwebDark = "#C42B97"

-- |Defines the color of a very light green.
numDark :: T.Text
numDark = "#B8FF70"

-- |Defines the color of a very light blue.
aiDark :: T.Text
aiDark = "#80B2FF"

-- |Defines the color of a soft lime green.
hciDark :: T.Text
hciDark = "#91F27A"

-- |Defines the color of a slightly desaturated violet.
mathDark :: T.Text
mathDark = "#8A67BE"

-- |Defines the color of a moderate cyan.
introDark :: T.Text
introDark = "#5DD5B8"

-- |Defines the color of a very dark blue.
titleColour :: T.Text
titleColour = "#072D68"

-- |Defines the color of a light gray.
lightGrey :: T.Text
lightGrey = "#CCCCCC"


{- Graph styles -}

-- |Defines node font size, 12 in pixels.
nodeFontSize :: Num a => a
nodeFontSize = 12

-- |Defines hybrid font size, 7 in pixels.
hybridFontSize :: Double
hybridFontSize = 7


-- |Defines bool font size, 6 in pixels.
boolFontSize :: Num a => a
boolFontSize = 6

-- |Defines region font size, 14 in pixels.
regionFontSize :: Num a => a
regionFontSize = 13
