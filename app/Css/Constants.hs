{-# LANGUAGE OverloadedStrings #-}

module Css.Constants
    (margin0,
     padding0,
     width100,
     height100,
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
     fceCountColor,
     nodeFontSize,
     hybridFontSize,
     boolFontSize,
     regionFontSize,
     pastelRed,
     pastelOrange,
     pastelYellow,
     pastelGreen,
     pastelBlue,
     pastelPink,
     pastelPurple,
     pastelBrown,     
     pastelGrey
     ) where

import Clay
import Prelude hiding ((**))
import Data.Text as T


{- Empty padding and margins. -}
margin0 :: Css
margin0 = margin nil nil nil nil
padding0 :: Css
padding0 = padding nil nil nil nil

{- Default rectangle height and width,
 - set to 100 percent. -}
width100 :: Css
width100 = width $ pct 100
height100 :: Css
height100 = height $ pct 100

{- Node and rectangle constants,
 - including sizes, strokes, fills,
 - opacities, colors and alignments. -}

-- Stroke & fill
fill :: Text -> Css
fill = (-:) "fill"
stroke :: Text -> Css
stroke = (-:) "stroke"

-- Center alignment
alignCenter :: Css
alignCenter = textAlign $ alignSide sideCenter
wideStroke :: Css
wideStroke = "stroke-width" -: "3"
faded :: Css
faded = opacity 0.4
semiVisible :: Css
semiVisible = opacity 0.7
fullyVisible :: Css
fullyVisible = opacity 1.0

strokeRed :: Css
strokeRed = do
    "stroke" -: "#CC0011"
    "stroke-width" -: "2px"
strokeDashed :: Css
strokeDashed = do
    "stroke-dasharray" -: "8,5"
    "stroke-width" -: "2px"

roundCorners :: Css
roundCorners = "border-radius" -: "8px"

-- Colors

theoryDark :: T.Text
theoryDark = "#B1C8D1"

coreDark :: T.Text
coreDark = "#C9C9C9"

seDark :: T.Text
seDark = "#E68080"

systemsDark :: T.Text
systemsDark = "#C285FF"

graphicsDark :: T.Text
graphicsDark = "#66A366"

dbwebDark :: T.Text
dbwebDark = "#C42B97"

numDark :: T.Text
numDark = "#B8FF70"

aiDark :: T.Text
aiDark = "#80B2FF"

hciDark :: T.Text
hciDark = "#91F27A"

mathDark :: T.Text
mathDark = "#8A67BE"

introDark :: T.Text
introDark = "#5DD5B8"

titleColour :: T.Text
titleColour = "#072D68"

lightGrey :: T.Text
lightGrey = "#CCCCCC"

{- Background colors. -}
purple1 :: Color
purple1 = parse "#46364A"
purple2 :: Color
purple2 = parse "#7E4D66"
purple3 :: Color
purple3 = parse "#CD96CD"
purple4 :: Color
purple4 = parse "#9C6B98"
purple5 :: Color
purple5 = parse "#800080"

purple6 :: Color
purple6 = parse "#CAC4D4"
purple7 :: Color
purple7 = parse "#9C91B0"
purple8 :: Color
purple8 = parse "#7A6A96"
purple9 :: Color
purple9 = parse "#433063"
purple10 :: Color
purple10 = parse "#5C497E"

pink1 :: Color
pink1 = parse "#DB94B8"
pink2 :: Color
pink2 = rgb 236 189 210

{- Empty/null border. Makes for a flat look. -}
borderNone :: Css
borderNone = border solid (px 0) white

{- Timetable border -}
borderPink :: (Stroke -> Size Abs -> Color -> Css) -> Css
borderPink borderStroke = borderStroke solid (px 2) pink1

{- More node colours! -}
teal1 :: Color
teal1 = parse "#737A99"
orange1 :: Color
orange1 = parse "#1E7FCC"

blue1 :: Color
blue1 = parse "#261B2A"
blue2 :: Color
blue2 = parse "#336685"
blue3 :: Color
blue3 = parse "#437699"
blue4 :: Color
blue4 = parse "#5566F5"
blue5 :: Color
blue5 = parse "#A5A6F5"
blue6 :: Color
blue6 = rgb 184 231 249
blueFb :: Color
blueFb = rgb 59 89 152

red1 :: Color
red1 = parse "#C92343"
red2 :: Color
red2 = parse "#B91333"

red3 :: Color
red3 = rgb 215 117 70
red4 :: Color
red4 = rgb 195 97 50
red5 :: Color
red5 = rgb 221 189 189

green1 :: Color
green1 = rgb 170 228 164
green2 :: Color
green2 = parse "#3Cb371"

dRed :: T.Text
dRed = "#D77546"

dGreen :: T.Text
dGreen = "#2E8B57"

dBlue :: T.Text
dBlue = "#437699"

dPurple :: T.Text
dPurple = "#46364A"

grey1 :: Color
grey1 = parse "#222"
grey2 :: Color
grey2 = parse "#dedede"
grey3 :: Color
grey3 = parse "#949494"
grey4 :: Color
grey4 = parse "#BABABA"
grey5 :: Color
grey5 = parse "#DCDCDC"
grey6 :: Color
grey6 = parse "#9C9C9C"

beige1 :: Color
beige1 = parse "#EBE8E4"

{-Color palette colors-}
pastelRed  :: Color
pastelRed = parse "#FF7878"

pastelOrange  :: Color
pastelOrange = parse "#FFC48C"

pastelYellow  :: Color
pastelYellow = parse "#EEDD99"

pastelGreen :: Color
pastelGreen = parse "#BDECB6"

pastelBlue :: Color
pastelBlue = parse "#9BD1FA"

pastelPink :: Color
pastelPink = parse "#FFD1DC"

pastelPurple :: Color
pastelPurple = parse "#E3AAD6"

pastelBrown :: Color
pastelBrown = parse "#AD876E"

pastelGrey :: Color
pastelGrey = parse "#A2A9AF"

{- FCE count color. Currently unused. -}
fceCountColor :: Color
fceCountColor = parse "#66C2FF"

{- Graph styles -}

-- Node font size, in pixels
nodeFontSize :: Num a => a
nodeFontSize = 12

hybridFontSize :: Num a => a
hybridFontSize = 7

boolFontSize :: Num a => a
boolFontSize = 6

regionFontSize :: Num a => a
regionFontSize = 14

