{-# LANGUAGE OverloadedStrings #-}

module Css.Draw
    (drawStyles) where

import Clay hiding (map, repeat, id)
import Prelude hiding ((**))
import Css.Constants
import qualified Data.Text as T

{- drawStyles
 - Generates all CSS for the draw page. -}
drawStyles :: Css
drawStyles = do
    colourTableCSS
    mainCSS
    titleDiv
    canvasCSS
    panelCSS
    modeButtonsCSS
    clickedButtonsCSS
    simpleButton
    inputCSS
    textButtonCSS
    nodeLabelCSS
    elbowCSS
    finishRegionCSS

{- The colour table. -}
colourTableCSS :: Css
colourTableCSS =
    "#colour-table" ? do
        height (px 40)
        width  (px 200)
        mapM_ makeRule [0..length colours - 1]
    where
        makeRule i =
            let colour = colours !! i
                (row, col) = divMod i 5
            in
                tr # nthChild (T.pack $ show $ row + 1) **
                td # nthChild (T.pack $ show $ col + 1) ? background colour

        colours = [
            pastelRed,    pastelYellow, pastelBlue,   pastelPink,  white,
            pastelOrange, pastelGreen,  pastelPurple, pastelBrown, pastelGrey
            ]

{- The wrapping around the canvas elements. -}
mainCSS :: Css
mainCSS = "#main" ? do
    height (pct 85)
    width  (pct 85)
    float floatRight
    position relative
    "border-radius" -: "8px"
    border solid (px 2) black

titleDiv :: Css
titleDiv = "#about-div" ? do
    fontSize (em 1.2)
    margin 0 0 0 (px 10)

{- The SVG canvas and the grid background. -}
canvasCSS :: Css
canvasCSS = do
    "#background" ? do
        height100
        width100
        "background-image" -: "url(/static/res/backgrounds/draw-background.png)"
        "background-size" -: "8px"
        opacity 0.3
    "#mySVG" ? do
        height100
        width100
        position absolute
        top nil
        left nil

{- The side panel. -}
panelCSS :: Css
panelCSS = do
    "#side-panel-wrap" ? do
        height (pct 85)
        width (pct 15)
        float floatLeft
        padding (px 5) 0 (px 5) 0
        border solid (px 2) black
        roundCorners
        backgroundColor $ parse "#008080"
        overflowY auto

{- The mode buttons. -}
modeButtonsCSS :: Css
modeButtonsCSS = ".mode" ? do
    width (pct 93)
    padding 0 0 0 (px 5)
    margin 0 0 0 (px 5)
    roundCorners
    fontSize (em 0.75)
    border solid (px 2) "#008080"
    "-webkit-transition" -: "all 0.2s"
    "-moz-transition" -: "all 0.2s"
    "-ms-transition" -: "all 0.2s"
    "-o-transition" -: "all 0.2s"
    "transition" -: "all 0.2s"
    ":hover" & do
        "background-color" -: "#28B0A2 !important"
        "color" -: "#DCDCDC !important"
        cursor pointer
    ".clicked" & do
        "background-color" -: "#28B0A2 !important"

clickedButtonsCSS :: Css
clickedButtonsCSS = ".clicked" ? do
    "color" -: "#DCDCDC !important"
    border solid (px 2) black

{- The input field. -}
inputCSS :: Css
inputCSS = "input" ? do
    fontSize (px 16)
    border solid (px 2) "#DCDCDC"
    roundCorners
    margin (px 5) (px 0) (px 5) (px 5)
    padding0
    ":focus" & do
        {-border solid (px 2) "#FFD700"-}
        "box-shadow" -: "0 0 3px 1px #FFD700"

{- Style for simple buttons. -}
simpleButton :: Css
simpleButton = ".button" ? do
    width (pct 40)
    margin (px 5) (px 5) (px 5) (px 5)
    padding0
    roundCorners
    alignCenter
    fontSize (em 0.75)
    border solid (px 2) "#008080"
    border solid (px 2) black
    "-webkit-transition" -: "all 0.2s"
    "-moz-transition" -: "all 0.2s"
    "-ms-transition" -: "all 0.2s"
    "-o-transition" -: "all 0.2s"
    "transition" -: "all 0.2s"
    ":hover" & do
        "background-color" -: "black !important"
        "color" -: "#DCDCDC !important"
        cursor pointer


{- The add button. -}
textButtonCSS :: Css
textButtonCSS = "#add-text" ? do
    "display" -: "inline"
    margin (px 5) (px 5) (px 5) (px 5)
    padding (px 2) (px 5) (px 2) (px 5)
    width (pct 45)


{- The labels for a node. -}
nodeLabelCSS :: Css
nodeLabelCSS = ".mylabel" ? do
    alignCenter
    "stroke" -: "none"
    userSelect none
    "-webkit-touch-callout" -: "none"
    "-webkit-user-select" -: "none"
    "-khtml-user-select" -: "none"
    "-moz-user-select" -: "none"
    "-ms-user-select" -: "none"
    "text-anchor" -: "middle"
    "dominant-baseline" -: "central"

{- The invisible elbow nodes. -}
elbowCSS :: Css
elbowCSS = do
    ".elbow" ? do
        opacity 0
        ":hover" & do
            cursor pointer
            opacity 1
    ".rElbow" ? do
        opacity 0
        ":hover" & do
            cursor pointer
            opacity 1

{- The finish button -}
finishRegionCSS :: Css
finishRegionCSS = "#finish-region" ? do
    width (pct 40)
    margin (px 5) (px 5) (px 5) (px 5)
    padding0
    backgroundColor $ parse "#DCDCDC"
    -- border solid (px 2) black
    border solid (px 2) $ parse "#008080"
