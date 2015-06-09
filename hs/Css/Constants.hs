{-# LANGUAGE OverloadedStrings #-}

module Css.Constants where

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

beige1 :: Color
beige1 = parse "#EBE8E4"

{- FCE count color. Currently unused. -}
fceCountColor :: Color
fceCountColor = parse "#66C2FF"
