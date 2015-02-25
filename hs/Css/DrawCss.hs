{-# LANGUAGE OverloadedStrings #-}

module DrawCss where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Constants

{- drawStyles
 - Generates all CSS for the draw page. -}

drawStyles = do
    mainCSS
    canvasBackground
    canvasCSS
    panelCSS
    modeButtonsCSS
    clickedButtonsCSS 
    colourButtonsCSS
    redCSS
    greenCSS
    blueCSS
    purpleCSS
    inputCSS
    textButtonCSS

mainCSS = "#main" ? do
    height (pct 85)
    width  (pct 85)
    float floatRight
    position relative
    "border-radius" -: "8px"
    border solid (px 2) "#000000"

canvasBackground = "#background" ? do
    height100
    width100
    "background-image" -: "url(http://www.emba.uvm.edu/~jtl/gimpdoc-html/plugab17.gif)"
    opacity 0.3

canvasCSS = "#mySVG" ? do
    height100
    width100
    position absolute
    top nil
    left nil

panelCSS = "#mode-panel" ? do
    height (pct 85)
    width (pct 15)
    backgroundColor $ parse "#008080"
    float floatLeft
    "border-radius" -: "8px"

modeButtonsCSS = ".mode" ? do
    width (pct 93)
    margin (px 5) (px 5) (px 5) (px 5)
    padding 0 (px 155) 0 (px 5)
    roundCorners
    fontSize (px 16)
    border solid (px 2) "#008080"
    ":hover" & do
        fontWeight bold
        cursor pointer
        textDecoration underline

clickedButtonsCSS = ".clicked" ? do
        fontWeight bold
        textDecoration underline
        border solid (px 2) "#000000"

colourButtonsCSS = ".colour" ? do
    width (pct 40)
    margin (px 5) (px 5) (px 5) (px 5)
    padding0
    roundCorners
    alignCenter
    fontSize (px 16)
    border solid (px 2) "#008080"
    ":hover" & do
        fontWeight bold
        cursor pointer
        textDecoration underline

redCSS = "#red" ? do
    backgroundColor $ parse dRed
    
greenCSS = "#green" ? do
    backgroundColor $ parse dGreen

blueCSS = "#blue"? do
    backgroundColor $ parse dBlue

purpleCSS = "#purple"? do
    backgroundColor $ parse dPurple

inputCSS = "input" ? do
    fontSize (px 16)
    border solid (px 2) "#dcdcdc"
    roundCorners
    margin (px 5) (px 0) (px 5) (px 5)
    padding0
    ":focus" & do
        border solid (px 2) "#FFD700"
        "box-shadow" -: "0 0 5px 1px #FFD700"

textButtonCSS = ".button" ? do
    "display" -: "inline"
    margin (px 5) (px 5) (px 5) (px 5)
    padding (px 2) (px 20) (px 2) (px 20)
    roundCorners
    alignCenter
    fontSize (px 16)
    border solid (px 2) "#dcdcdc"
    ":hover" & do
        cursor pointer