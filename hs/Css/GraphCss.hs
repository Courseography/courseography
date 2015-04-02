{-# LANGUAGE OverloadedStrings #-}

module Css.GraphCss where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

{- graphStyles
 - Generates all CSS for the graph page. -}

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
            fontSize (pt 12)
            faded
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
            wideStroke
            strokeRed
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
    ".bool" & do
        cursor cursorDefault
        "data-active" @= "active" & do
            "ellipse" <? do
                fill "white"
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
        "data-active" @= "takeable" & do
            "ellipse" <? do
                fill lightGrey
        "data-active" @= "missing" & do
            "ellipse" <? do
                fill "white"
                strokeRed
        "data-active" @= "unlit" & do
            wideStroke
            strokeRed
        "text" <? do
            fontFamily ["Comic Sans MS"] [sansSerif]
            fontWeight bold

{- pathCSS
 - Generates CSS for paths between nodes
 - in the graph. -}

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
        faded
        strokeRed
        strokeDashed
    "data-active" @= "drawn" & do
        faded
        wideStroke
    "data-active" @= "region" & do
        faded
        "stroke-width" -: "0"


{- resetCSS
 - Generates CSS for the reset feature
 - in the graph. -}

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

graphContainer = do
    "#graph" ? do
        width (px 1100)
        minHeight (px 700)
        height (px 700)
        overflow hidden
        margin nil auto nil auto
        clear both
        display inlineBlock
        position absolute
        textAlign $ alignSide sideCenter
        "left" -: "10%"
    "#graphRootSVG" ? do
        width100
        height100
        stroke "black"
        "stroke-linecap" -: "square"
        "stroke-miterlimit" -: "10"
        "shape-rendering" -: "geometricPrecision"

sidebarCSS = do
    "#fcecount" ? do
        width (pct 100)
        height (px 30)
        backgroundColor purple4
        border solid (px 1) black
        textAlign $ alignSide sideCenter
        display none
    "#container" ? do
        width (pct 100)
        height (px 700)
        position relative
    "#sidebar" ? do
        display inlineBlock
        width (px 40)
        height (pct 100)
        float floatLeft
        backgroundColor purple5
        position absolute
        paddingLeft (px 23)
    "#sidebar-button" ? do
        cursor pointer
        display inlineBlock
        width (px 40)
        height (pct 100)
        float floatLeft
        backgroundColor purple2
        position absolute
        ":hover" & do
            backgroundColor purple4
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
        width (pct 100)
        fontSize (px 13)
        backgroundColor purple3
        border solid (px 1) grey2
        "box-shadow" -: "0 2px 2px -1px rgba(0, 0, 0, 0.055)"
        display block
        overflow hidden
        ul ? do
            width $ (pct 100)
            margin0
            li ? do
                "list-style-type" -: "none"
                display inlineBlock
                width (pct 48)
                --textAlign $ alignSide sideCenter
                "-webkit-transition" -: "all 0.2s"
                "-moz-transition" -: "all 0.2s"
                "-ms-transition" -: "all 0.2s"
                "-o-transition" -: "all 0.2s"
                "transition" -: "all 0.2s"
                ":hover" & do
                    "background-color" -: "#46364A !important"
                    a ? do
                        "color" -: "white !important"
                a ? do
                    color black
                    display inlineBlock
                    lineHeight (px 30)
                    --paddingLeft (px 26)
                    textAlign $ alignSide sideCenter
                    width (pct 95)
                    textDecoration none
    "#focuses, #graphs" ? do
        marginTop (px 25)
        height (pct 100)
        width (pct 100)
        display none
        marginLeft (px 25)
    ".focus" ? do
        display block
        cursor pointer
        fontSize (px 20)
        border solid (px 1) black
        textAlign $ alignSide sideCenter
        width (pct 90)
        backgroundColor white
        "border-radius" -: "6px"
        ":hover" & do
            backgroundColor grey2
    "#close-focus" ? do
        display block
        cursor pointer
        backgroundColor purple3
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
        backgroundColor purple1
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

titleCSS = "#svgTitle" ? do
    fontSize $ em 2.5
    fontWeight bold
    fontFamily ["Bitter"] [serif]
    fontStyle italic
    fill titleColour

{- regionCSS
 - Generates CSS for focus regions in the graph. -}

regionCSS = ".region-label" ? do
    "text-anchor" -: "start"
