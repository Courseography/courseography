{-# LANGUAGE OverloadedStrings #-}

module Css.DrawCss where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

{- drawStyles
 - Generates all CSS for the draw page. -}

drawStyles = do
    mainCSS
    canvasCSS
    panelCSS
    modeButtonsCSS
    colourButtonsCSS
    clickedButtonsCSS 
    simpleButton
    inputCSS
    textButtonCSS
    nodeLabelCSS
    elbowCSS
    scrollBar
    regionCSS 
    finishRegionCSS
    saveButtonCSS
    saveModalCSS

{- The wrapping around the canvas elements. -}
mainCSS = "#main" ? do
    height (pct 85)
    width  (pct 85)
    float floatRight
    position relative
    "border-radius" -: "8px"
    border solid (px 2) black

{- The SVG canvas and the grid background. -}
canvasCSS = do
    "#background" ? do
        height100
        width100
        "background-image" -: "url(/static/res/grid.gif)"
        "background-size" -: "128px"
        opacity 0.3
    "#mySVG" ? do
        height100
        width100
        position absolute
        top nil
        left nil

{- The side panel wrapping. -}
panelCSS = do
    "#side-panel-wrap" ? do
        height (pct 85)
        width (pct 15)
        float floatLeft
        padding (px 5) 0 (px 5) 0
        border solid (px 2) black
        roundCorners
        backgroundColor $ parse "#008080"
    "#mode-panel" ? do
        height (pct 100)
        width (pct 100)
        "overflow-y" -: "auto"

{- Override the default scrollbar styling for side panel -}
scrollBar = do
    "::-webkit-scrollbar" ? do
        width (px 10)
        height (px 10)
    "::-webkit-scrollbar-track" ? do
        "-webkit-box-shadow" -: "inset 0 0 6px rgba(0,0,0,1)"
        "border-radius" -: "10px"
    "::-webkit-scrollbar-thumb" ? do 
        "border-radius" -: "10px";
        "-webkit-box-shadow" -: "inset 0 0 6px rgba(0,0,0,0.5)"
        "background-color" -: "#28B0A2"  

{- The mode buttons. -}
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

clickedButtonsCSS = ".clicked" ? do
    "color" -: "#DCDCDC !important"
    border solid (px 2) black

{- The colour buttons. -}
colourButtonsCSS = do
    ".colour" ? do
        width (pct 40)
        margin (px 5) (px 5) (px 5) (px 5)
        padding0
        roundCorners
        alignCenter
        fontSize (em 0.75)
        border solid (px 2) "#008080"
        "-webkit-transition" -: "all 0.2s"
        "-moz-transition" -: "all 0.2s"
        "-ms-transition" -: "all 0.2s"
        "-o-transition" -: "all 0.2s"
        "transition" -: "all 0.2s"
        ":hover" & do
            "background-color" -: "black !important"
            "color" -: "#DCDCDC !important"
            cursor pointer
    "#red" ? do
        backgroundColor $ parse dRed
    "#green" ? do
        backgroundColor $ parse dGreen
    "#blue"? do
        backgroundColor $ parse dBlue
    "#purple"? do
        backgroundColor $ parse dPurple

{- Style for simple buttons. -}
simpleButton = ".button" ? do
    width (pct 40)
    roundCorners
    alignCenter
    fontSize (em 0.75)
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

{- The input field. -}
inputCSS = "input" ? do
    fontSize (px 16)
    border solid (px 2) "#DCDCDC"
    roundCorners
    margin (px 5) (px 0) (px 5) (px 5)
    padding0
    ":focus" & do
        {-border solid (px 2) "#FFD700"-}
        "box-shadow" -: "0 0 3px 1px #FFD700"

{- The add button. -}
textButtonCSS = "#add-text" ? do
    "display" -: "inline"
    margin (px 5) (px 5) (px 5) (px 5)
    padding (px 2) (px 5) (px 2) (px 5)
    width (pct 45)

{- The labels for a node. -}
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

{- The actual region svg elements. -}
regionCSS = ".region" ? do
    "data-group" @= "red" & do
        fill dRed
    "data-group" @= "blue" & do
        fill dBlue
    "data-group" @= "green" & do
        fill dGreen
    "data-group" @= "purple" & do
        fill dPurple

{- The finish button. -}
finishRegionCSS = "#finish-region" ? do
    width (pct 40)
    margin (px 5) (px 5) (px 5) (px 5)
    padding0
    backgroundColor $ parse "#DCDCDC"
    border solid (px 2) $ parse "#008080"

saveButtonCSS = do
    "#save" ? do
        width (pct 40)
        "margin" -: "15px auto 5px auto"
        padding0
    "#save-modal-button" ? do
        "display" -: "inline"
        width (pct 50)
        padding (px 2) (px 10) (px 2) (px 10)
        margin (px 2) (px 5) (px 5) (px 2)

saveModalCSS = do
    "#save-modal-blackout" ? do
        position fixed
        width100
        height100
        top (px 0)
        left (px 0)
        backgroundColor black
        opacity 0.8
    "#save-modal" ? do
        position relative
        width (pct 50)
        height (pct 20)
        padding (px 5) (px 5) (px 5) (px 5)
        "margin" -: "50px auto 50px auto"
        roundCorners
        border solid (px 2) black
        backgroundColor $ parse "#008080"
    "#save-modal-subdiv" ? do
        position absolute