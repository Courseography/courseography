{-# LANGUAGE OverloadedStrings #-}

module Css.Constants where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Data.Text as T


{- Empty padding and margins. -}
margin0 = margin nil nil nil nil
padding0 = padding nil nil nil nil

{- Default rectangle height and width,
 - set to 100 percent. -}
width100 = width $ pct 100
height100 = height $ pct 100

{- Node and rectangle constants,
 - including sizes, strokes, fills,
 - opacities, colors and alignments. -}

-- Stroke & fill
fill = (-:) "fill"
stroke = (-:) "stroke"

-- Center alignment
alignCenter = textAlign $ alignSide sideCenter

wideStroke = "stroke-width" -: "3"
faded = opacity 0.4
semiVisible = opacity 0.7
fullyVisible = opacity 1.0
strokeRed = do
    "stroke" -: "#CC0011"
    "stroke-width" -: "2px"
strokeDashed = do
    "stroke-dasharray" -: "8,5"
    "stroke-width" -: "2px"

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

titleColour :: T.Text
titleColour = "#072D68"

lightGrey :: T.Text
lightGrey = "#CCCCCC"

{- Color for the course modal. -}
modalColor = parse "#374AA1"

{- Background colors. -}

purple1 = parse "#46364A"
purple2 = parse "#7E4D66"
pink1 = parse "#DB94B8"

{- Empty/null border. Makes for a flat look. -}
borderNone = border solid (px 0) white

{- More node colours! -}
teal1 = parse "#737A99"
orange1 = parse "#1E7FCC"

blue1 = parse "#261B2A"
blue2 = parse "#336685"
blue3 = parse "#437699"

blue4 = parse "#5566F5"

red1 = parse "#C92343"
red2 = parse "#B91333"

red3 = rgb 215 117 70
red4 = rgb 195 97 50

dRed :: T.Text
dRed = "#D77546"

dGreen :: T.Text
dGreen = "#2E8B57"

dBlue :: T.Text
dBlue = "#437699"

dPurple :: T.Text
dPurple = "#46364A"

grey1 = parse "#222"
grey2 = parse "#dedede"
grey3 = parse "#949494"
grey4 = parse "#BABABA"
grey5 = parse "#DCDCDC"

beige1 = parse "#EBE8E4"

{- FCE count color. Currently unused. -}
fceCountColor = parse "#66C2FF"
