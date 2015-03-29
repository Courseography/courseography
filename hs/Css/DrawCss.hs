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
    canvasBackground
    canvasCSS
    panelWrap
    panelCSS
    modeButtonsCSS
    colourButtonsCSS
    redCSS
    greenCSS
    blueCSS
    purpleCSS
    clickedButtonsCSS 
    inputCSS
    textButtonCSS
    textCSS
    elbowCSS
    scrollBar
    regionCSS 
    finishRegionCSS

{- The wrapping around the canvas elements. -}
mainCSS = "#main" ? do
    height (pct 85)
    width  (pct 85)
    float floatRight
    position relative
    "border-radius" -: "8px"
    border solid (px 2) black

{- The grid background for SVG canvas. -}
canvasBackground = "#background" ? do
    height100
    width100
    "background-image" -: "url(/static/res/grid.gif)"
    "background-size" -: "128px"
    opacity 0.3

{- The SVG canvas. -}
canvasCSS = "#mySVG" ? do
    height100
    width100
    position absolute
    top nil
    left nil

{- The side panel wrapping. -}
panelWrap = "#side-panel-wrap" ? do
    height (pct 85)
    width (pct 15)
    float floatLeft
    padding (px 5) 0 (px 5) 0
    border solid (px 2) black
    roundCorners
    backgroundColor $ parse "#008080"

{- The side panel. -}
panelCSS = "#mode-panel" ? do
    height (pct 100)
    width (pct 100)
    "overflow-y" -: "auto"

{- Override the default scrollbar styling for side panel -}
scrollBar = do
    scroll1 
    scroll2 
    scroll3

scroll1 = "::-webkit-scrollbar" ? do
        width (px 10)
        height (px 10)
 
scroll2 = "::-webkit-scrollbar-track" ? do
        "-webkit-box-shadow" -: "inset 0 0 6px rgba(0,0,0,1)"
        "border-radius" -: "10px"

scroll3 = "::-webkit-scrollbar-thumb" ? do 
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
colourButtonsCSS = ".colour" ? do
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

{- Background colours for colour buttons. -}
redCSS = "#red" ? do
    backgroundColor $ parse dRed
    
greenCSS = "#green" ? do
    backgroundColor $ parse dGreen

blueCSS = "#blue"? do
    backgroundColor $ parse dBlue

purpleCSS = "#purple"? do
    backgroundColor $ parse dPurple

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
textButtonCSS = ".button" ? do
    "display" -: "inline"
    margin (px 5) (px 5) (px 5) (px 5)
    padding (px 2) (px 2) (px 2) (px 2)
    width (pct 45)
    roundCorners
    alignCenter
    fontSize (em 0.75)
    border solid (px 2) black
    ":hover" & do
        cursor pointer

{- The labels for a node. -}
textCSS = ".mylabel" ? do
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

{- The finish button -}
finishRegionCSS = "#finish-region" ? do
    width (pct 40)
    margin (px 5) (px 5) (px 5) (px 5)
    padding0
    roundCorners
    alignCenter
    fontSize (em 0.75)
    backgroundColor $ parse "#DCDCDC"
    -- border solid (px 2) black
    "-webkit-transition" -: "all 0.2s"
    "-moz-transition" -: "all 0.2s"
    "-ms-transition" -: "all 0.2s"
    "-o-transition" -: "all 0.2s"
    "transition" -: "all 0.2s"
    ":hover" & do
        "background-color" -: "black !important"
        "color" -: "#DCDCDC !important"
        cursor pointer
