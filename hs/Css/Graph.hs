{-# LANGUAGE OverloadedStrings #-}

module Css.Graph where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

{- graphStyles
 - Generates all CSS for the graph page. -}
graphStyles :: Css
graphStyles = do
    graphContainer
    sidebarCSS
    nodeCSS
    pathCSS
    resetCSS
    titleCSS
    regionCSS

{- nodeCSS
 - Generates CSS for nodes in the graph. -}
nodeCSS :: Css
nodeCSS = "g" ? do
    "text" ? do
        userSelect none
        "-webkit-touch-callout" -: "none"
        "-webkit-user-select" -: "none"
        "-khtml-user-select" -: "none"
        "-moz-user-select" -: "none"
        "-ms-user-select" -: "none"
    ".node" & do
        cursor pointer
        "text" ? do
            fontSize (pt nodeFontSize)
            faded
            stroke "none"
            "text-anchor" -: "middle"
        "data-active" @= "active" & do
            "rect" <? do
                wideStroke
            "text" <? do
                fullyVisible
        "data-active" @= "overridden" & do
            "rect" <? do
                wideStroke
                strokeRed
            "text" <? do
                fullyVisible
        "data-active" @= "inactive" & do
            "rect" <? do
                faded
                strokeDashed
        "data-active" @= "takeable" & do
            "rect" <? do
                semiVisible
            "text" <? do
                semiVisible
        "data-active" @= "missing" & do
            "rect" <> "ellipse" <? do
                wideStroke
                strokeRed
            "text" <? do
                fullyVisible
        "data-active" @= "unlit" & do
            "rect" <?
                faded
        "data-active" @= "unselected" & do
            "rect" <? do
                wideStroke
                faded
        -- For nodes in draw tab
        "data-group" @= "red" & do
            "rect" <? do
                fill dRed
        "data-group" @= "blue" & do
            "rect" <? do
                fill dBlue
        "data-group" @= "green" & do
            "rect" <? do
                fill dGreen
        "data-group" @= "purple" & do
            "rect" <? do
                fill dPurple
        "rect" <? do
            stroke "black"
    ".hybrid" & do
        cursor cursorDefault
        "text" <? do
            stroke "none"
            fill "white"
            fontSize (pt hybridFontSize)
            "text-anchor" -: "middle"
        "rect" <? do
            fill "#888888"
            stroke "black"
    ".bool" & do
        cursor cursorDefault
        "data-active" @= "active" & do
            "ellipse" <? do
                fill "none"
                stroke "black"
        "data-active" @= "overridden" & do
            "ellipse" <? do
                fill "white"
                strokeRed
        "data-active" @= "inactive" & do
            "ellipse" <? do
                fill lightGrey
                faded
                strokeDashed
                stroke "black"
        "data-active" @= "takeable" & do
            "ellipse" <? do
                fill lightGrey
                stroke "black"
        "data-active" @= "missing" & do
            "ellipse" <? do
                fill "white"
                strokeRed
        "data-active" @= "unlit" & do
            wideStroke
            strokeRed
        "text" <? do
            fontFamily ["Trebuchet MS", "Arial"] [sansSerif]
            fontWeight bold
            stroke "none"
            fontSize (pt boolFontSize)
            "text-anchor" -: "middle"

{- pathCSS
 - Generates CSS for paths between nodes
 - in the graph. -}
pathCSS :: Css
pathCSS = "path" ? do
    fill "none"
    "data-active" @= "takeable" & do
        strokeDashed
    "data-active" @= "inactive" & do
        faded
        strokeDashed
    "data-active" @= "active" & do
        opacity 1
        "stroke-width" -: "2px"
    "data-active" @= "missing" & do
        strokeRed
        strokeDashed
    "data-active" @= "drawn" & do
        faded
        wideStroke


{- resetCSS
 - Generates CSS for the reset feature
 - in the graph. -}
resetCSS :: Css
resetCSS = "#resetButton" ? do
    fill "#990000"
    cursor pointer
    wideStroke
    "stroke" -: "#404040"
    "text" <? do
        fill "white"

{- graphContainer
 - Generates CSS for the main division of
 - the page containing the graph. -}
graphContainer :: Css
graphContainer = do
    "#graph" ? do
        width (px 1195)
        minHeight (px 700)
        height (px 700)
        overflow hidden
        margin (px 10) (px 10) (px 10) (px 10)
        display inlineBlock
        position absolute
        textAlign $ alignSide sideCenter
        "left" -: "40px"
    "#graphRootSVG" ? do
        width100
        height100
        stroke "black"
        "stroke-linecap" -: "square"
        "stroke-miterlimit" -: "10"
        "shape-rendering" -: "geometricPrecision"

sidebarCSS :: Css
sidebarCSS = do
    "#fce" ? do
        height (px 40)
        width (pct 100)
        border solid (px 1) black
    "#fcecount" ? do
        width (pct 53)
        height (px 40)
        float floatLeft
        backgroundColor steelblue2
        display none
        textAlign $ alignSide sideCenter
        paddingLeft (px 15)
        paddingTop (px 5)
        fontSize (px 18)
    "#reset" ? do
        textAlign $ alignSide sideCenter
        float floatLeft
        width (pct 47)
        height (px 40)
        display none
        border solid (px 1) black
        cursor pointer
        ":hover" & do
            backgroundColor grey1
            fontColor white
    "#container" ? do
        width (pct 100)
        height (px 700)
        position relative
    "#sidebar" ? do
        display inlineBlock
        width (px 40)
        height (pct 100)
        float floatLeft
        backgroundColor steelblue4
        position absolute
        paddingLeft (px 23)
    "#sidebar-button" ? do
        cursor pointer
        display inlineBlock
        width (px 40)
        height100
        float floatLeft
        backgroundColor steelblue3
        position absolute
        border solid (px 1) black
        ":hover" & do
            backgroundColor steelblue2
    "#sidebar-icon" ? do
        width (px 30)
        height (px 35)
        paddingTop (px 20)
        paddingLeft (px 2)
        position absolute
        top (pct 40)
        left (px 4)
    "#focuses-nav, #graph-nav" ? do
        cursor pointer
    "#sidebar-nav" ? do
        width100
        fontSize (px 13)
        backgroundColor steelblue1
        border solid (px 1) grey2
        "box-shadow" -: "0 2px 2px -1px rgba(0, 0, 0, 0.055)"
        display block
        overflow hidden
        ul ? do
            width100
            margin0
            li ? do
                "list-style-type" -: "none"
                display inlineBlock
                width (pct 48)
                "-webkit-transition" -: "all 0.2s"
                "-moz-transition" -: "all 0.2s"
                "-ms-transition" -: "all 0.2s"
                "-o-transition" -: "all 0.2s"
                "transition" -: "all 0.2s"
                ":hover" & do
                    "background-color" -: "#4C527F !important"
                    a ? do
                        "color" -: "white !important"
                a ? do
                    color black
                    display inlineBlock
                    lineHeight (px 30)
                    alignCenter
                    width (pct 95)
                    textDecoration none
    "#focuses, #graphs" ? do
        marginTop (px 25)
        marginLeft (px 25)
        height100
        width100
        display none
    ".focus" ? do
        display block
        cursor pointer
        fontSize (px 20)
        border solid (px 1) black
        alignCenter
        width (pct 90)
        backgroundColor white
        "border-radius" -: "6px"
        ":hover" & do
            backgroundColor grey2
    "#close-focus" ? do
        display block
        cursor pointer
        backgroundColor steelblue1
        fontSize (px 20)
        border solid (px 1) black
        textAlign $ alignSide sideCenter
        width (pct 90)
    ".spotlight" & do
        semiVisible
        fill "white"
        stroke "none"
    ".details" & do
        border solid (px 1) black
        width (pct 90)
        height (px 0)
        marginBottom (px 7)
        overflow auto
        fontSize (px 14)
        fontColor white
        paddingLeft (px 5)
        paddingRight (px 5)
    ".active" & do
        backgroundColor steelblue3
    ".graph-button" & do
        display block
        cursor pointer
        fontSize (px 20)
        border solid (px 1) black
        "border-radius" -: "6px"
        textAlign $ alignSide sideCenter
        width (pct 90)
        backgroundColor white
        marginBottom (px 20)
        ":hover" & do
            backgroundColor grey2


{- titleCSS
 - Generates CSS for the title. -}
titleCSS :: Css
titleCSS = "#svgTitle" ? do
    fontSize $ em 2.5
    fontWeight bold
    fontFamily ["Bitter"] [serif]
    fontStyle italic
    fill titleColour

{- regionCSS
 - Generates CSS for focus regions in the graph. -}
regionCSS :: Css
regionCSS = do
    "#region-labels > text" ? do
        "text-anchor" -: "start"
        fontSize (pt regionFontSize)
    ".region" ? do
        "fill-opacity" -: "0.25"
