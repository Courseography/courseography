{-|
    Module      : Css.Constants
    Description : Defines the constants for the other CSS modules.
-}
module Css.Constants
    (margin0,
     padding0,
     width100,
     height100,
     -- * Node and Rectangle Constants
     fill,
     stroke,
     alignCenter,
     wideStroke,
     faded,
     semiVisible,
     fullyVisible,
     strokeRed,
     strokeDashed,
     roundCorners,
     -- * Colors
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
     -- * Background Colors
     purple1,
     purple2,
     purple3,
     purple4,
     purple5,
     purple6,
     purple7,
     purple8,
     purple9,
     purple10,
     pink1,
     pink2,
     borderNone,
     borderPink,
     -- * More Node Colors!
     teal1,
     orange1,
     blue1,
     blue2,
     blue3,
     blue4,
     blue5,
     blue6,
     blueFb,
     red1,
     red2,
     red3,
     red4,
     red5,
     green1,
     green2,
     dRed,
     dGreen,
     dBlue,
     dPurple,
     grey1,
     grey2,
     grey3,
     grey4,
     grey5,
     grey6,
     beige1,
     -- * Color Palette Colors
     pastelRed,
     pastelOrange,
     pastelYellow,
     pastelGreen,
     pastelBlue,
     pastelPink,
     pastelPurple,
     pastelBrown,
     pastelGrey,
     -- * FCE Count Color
     fceCountColor,
     -- * PostPage Color
     darkRose,
     softGreen,
     mGreen,
     mRed,
     -- * Graph Styles
     nodeFontSize,
     hybridFontSize,
     boolFontSize,
     regionFontSize
     ) where

import Clay
import Prelude hiding ((**))
import Data.Text as T

-- |Defines CSS for empty margins.
margin0 :: Css
margin0 = margin nil nil nil nil

-- |Defines CSS for empty padding.
padding0 :: Css
padding0 = padding nil nil nil nil

-- |Defines default rectangle width, which is 100%.
width100 :: Css
width100 = width $ pct 100

-- |Defines default rectangle height, which is 100%.
height100 :: Css
height100 = height $ pct 100

{- Node and rectangle constants,
 - including sizes, strokes, fills,
 - opacities, colors and alignments. -}

-- |Defines "fill" as text for CSS.
fill :: Text -> Css
fill = (-:) "fill"

-- |Defines "stroke" as text for CSS.
stroke :: Text -> Css
stroke = (-:) "stroke"

-- |Defines the CSS for center alignment for a node or rectangle.
alignCenter :: Css
alignCenter = textAlign $ alignSide sideCenter

-- |Defines the CSS for a wide stroke.
wideStroke :: Css
wideStroke = "stroke-width" -: "3"

-- |Defines the CSS for a lower opacity, called faded.
faded :: Css
faded = opacity 0.4

-- |Defines the CSS for a mid-high opacity, but not quite opaque.
semiVisible :: Css
semiVisible = opacity 0.7

-- |Defines the CSS for something that is opaque.
fullyVisible :: Css
fullyVisible = opacity 1.0

-- |Defines the CSS for a strong red stroke.
strokeRed :: Css
strokeRed = do
    "stroke" -: "#CC0011"
    "stroke-width" -: "2px"

{-|
 Defines the CSS for a dashed stroke, with the width between dashes being
 a bit smaller than the dash itself.
-}
strokeDashed :: Css
strokeDashed = do
    "stroke-dasharray" -: "8,5"
    "stroke-width" -: "2px"

-- |Defines the CSS for the rounded corners of a border.
roundCorners :: Css
roundCorners = "border-radius" -: "8px"

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

{- Background colors. -}

-- |Defines the color of a dark grayish magenta, intended for the background.
purple1 :: Color
purple1 = parse "#46364A"

-- |Defines the color of a mostly desaturated dark pink, intended for the
-- background.
purple2 :: Color
purple2 = parse "#7E4D66"

-- |Defines the color of a slightly desaturated magenta, intended for the
-- background.
purple3 :: Color
purple3 = parse "#CD96CD"

-- |Defines the color of a mostly desaturated dark magenta, intended for the
-- background.
purple4 :: Color
purple4 = parse "#9C6B98"

-- |Defines the color of a dark magenta, intended for the background.
purple5 :: Color
purple5 = parse "#800080"

-- |Defines the color of a grayish violet, intended for the background.
purple6 :: Color
purple6 = parse "#CAC4D4"

-- |Defines the color of a dark grayish violet, intended for the background.
purple7 :: Color
purple7 = parse "#9C91B0"

-- |Defines the color of a mostly desaturated dark violet, intended for the
-- background.
purple8 :: Color
purple8 = parse "#7A6A96"

-- |Defines the color of a very dark desaturated violet, intended for the
-- background.
purple9 :: Color
purple9 = parse "#433063"

-- |Defines the color of a mostly desaturated dark violet, intended for the
-- background.
purple10 :: Color
purple10 = parse "#5C497E"

-- |Defines the color of a very soft pink, intended for the background.
pink1 :: Color
pink1 = parse "#DB94B8"

-- |Defines the color of a light grayish pink, intended for the background.
pink2 :: Color
pink2 = rgb 236 189 210

-- |Defines an empty border, making for a flat look.
borderNone :: Css
borderNone = border solid (px 0) white

-- |Defines a border with a color of pink1, intended for the timetable.
borderPink :: (Stroke -> Size LengthUnit -> Color -> Css) -> Css
borderPink borderStroke = borderStroke solid (px 2) pink1

{- More node colours! -}

-- |Defines the color of a dark grayish blue, intended for nodes.
teal1 :: Color
teal1 = parse "#737A99"

-- |Defines the color of a strong blue, intended for nodes.
orange1 :: Color
orange1 = parse "#1E7FCC"

-- |Defines the color of a very dark, mostly black, violet intended for nodes.
blue1 :: Color
blue1 = parse "#261B2A"

-- |Defines the color of a dark moderate blue, intended for nodes.
blue2 :: Color
blue2 = parse "#336685"

-- |Defines the color of a slightly lighter than blue2 dark moderate blue,
-- intended for nodes.
blue3 :: Color
blue3 = parse "#437699"

-- |Defines the color of a soft blue, intended for nodes.
blue4 :: Color
blue4 = parse "#5566F5"

-- |Defines the color of a very soft blue, intended for nodes.
blue5 :: Color
blue5 = parse "#A5A6F5"

-- |Defines the color of a slightly lighter than blue5 very soft blue,
-- intended for nodes.
blue6 :: Color
blue6 = rgb 184 231 249

-- |Defines the color of a slightly more virbrant than blue2 dark moderate
-- blue, intended for nodes.
blueFb :: Color
blueFb = rgb 59 89 152

-- |Defines the color of a strong red, intended for nodes.
red1 :: Color
red1 = parse "#C92343"

-- |Defines the color of a darker than red1 strong red, intended for nodes.
red2 :: Color
red2 = parse "#B91333"

-- |Defines the color of a moderate orange, intended for nodes.
red3 :: Color
red3 = rgb 215 117 70

-- |Defines the color of a slightly darker than red3 moderate orange, intended
-- for nodes.
red4 :: Color
red4 = rgb 195 97 50

-- |Defines the color of a light grayish red, intended for nodes.
red5 :: Color
red5 = rgb 221 189 189

-- |Defines the color of a very soft lime green, intended for nodes.
green1 :: Color
green1 = rgb 170 228 164

-- |Defines the color of a moderate cyan - lime green, intended for nodes.
green2 :: Color
green2 = parse "#3Cb371"

-- |Defines the color of a slightly darker than red4 moderate orange, intended
-- for nodes
dRed :: T.Text
dRed = "#D77546"

-- |Defines the color of a dark moderate cyan - lime green, intended for
-- nodes.
dGreen :: T.Text
dGreen = "#2E8B57"

-- |Defines the color of a dark moderate blue, intended for nodes.
dBlue :: T.Text
dBlue = "#437699"

-- |Defines the color of a very dark grayish magenta, intended for nodes.
dPurple :: T.Text
dPurple = "#46364A"

-- |Defines the color of a very dark gray, mostly black, intended for nodes.
grey1 :: Color
grey1 = parse "#222"

-- |Defines the color of a very light gray, intended for nodes.
grey2 :: Color
grey2 = parse "#dedede"

-- |Defines the color of a dark gray, intended for nodes.
grey3 :: Color
grey3 = parse "#949494"

-- |Defines the color of a gray, intended for nodes.
grey4 :: Color
grey4 = parse "#BABABA"

-- |Defines the color of a slightly darker grey2 very light gray, intended for
-- nodes.
grey5 :: Color
grey5 = parse "#DCDCDC"

-- |Defines the color of a slightly lighter than grey3 dark gray, intended for
-- nodes.
grey6 :: Color
grey6 = parse "#9C9C9C"

-- |Defines the color of a light grayish orange, intended for nodes.
beige1 :: Color
beige1 = parse "#EBE8E4"

{-Color palette colors-}

-- |Defines the color of a very light red.
pastelRed  :: Color
pastelRed = parse "#FF7878"

-- |Defines the color of a very light orange.
pastelOrange  :: Color
pastelOrange = parse "#FFC48C"

-- |Defines the color of a very soft yellow.
pastelYellow  :: Color
pastelYellow = parse "#EEDD99"

-- |Defines the color of a very soft lime green.
pastelGreen :: Color
pastelGreen = parse "#BDECB6"

-- |Defines the color of a very soft blue.
pastelBlue :: Color
pastelBlue = parse "#9BD1FA"

-- |Defines the color of a very pale red.
pastelPink :: Color
pastelPink = parse "#FFD1DC"

-- |Defines the color of a very soft magenta.
pastelPurple :: Color
pastelPurple = parse "#E3AAD6"

-- |Defines the color of a mostly desaturated dark orange.
pastelBrown :: Color
pastelBrown = parse "#AD876E"

-- |Defines the color of a dark grayish blue.
pastelGrey :: Color
pastelGrey = parse "#A2A9AF"

{- FCE count color. Currently unused. -}

-- |Defines the color of a light blue, intended for FCE count, and currently
-- unused.
fceCountColor :: Color
fceCountColor = parse "#66C2FF"

{- PostPage Color -}

-- |Defines the color of a soft dark green for post credits
mGreen :: Color
mGreen = parse "#519A73"

-- |Defines the color of a soft dark red for post credits
mRed :: Color
mRed = parse "#C91F37"

-- |Defines the color of a dark rose for bottom bar
darkRose :: Color
darkRose = parse "#815463"

-- |Defines the color of a soft dark rose for selected courses
softGreen :: Color
softGreen = parse "#669966"

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
regionFontSize = 14
