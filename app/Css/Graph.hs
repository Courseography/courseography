{-# LANGUAGE OverloadedStrings #-}

module Css.Graph
    (graphStyles) where

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
        -- For nodes in draw tab
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
        "data-active" @= "unselected" & do
            "rect" <? do
                wideStroke
                faded
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
    ".active" & do
        "rect" <? do
            wideStroke
        "text" <? do
            fullyVisible
    ".overridden" & do
        "rect" <? do
            wideStroke
            strokeRed
        "text" <? do
            fullyVisible
    ".inactive" & do
        "rect" <? do
            faded
            strokeDashed
    ".takeable" & do
        "rect" <? do
            semiVisible
        "text" <? do
            semiVisible
    ".missing" & do
        "rect" <> "ellipse" <? do
            wideStroke
            strokeRed
        "text" <? do
            fullyVisible
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
        -- For the React graph
        ".active" & do
            "ellipse" <? do
                fill "none"
                stroke "black"
        ".overridden" & do
            "ellipse" <? do
                fill "white"
                strokeRed
        ".inactive" & do
            "ellipse" <? do
                fill lightGrey
                faded
                strokeDashed
                stroke "black"
        ".takeable" & do
            "ellipse" <? do
                fill lightGrey
                stroke "black"
        ".missing" & do
            "ellipse" <? do
                fill "white"
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
    "data-active" @= "drawn" & do
        faded
        wideStroke
    -- For the React graph
    ".takeable" & do
        strokeDashed
        stroke "black"
    ".inactive" & do
        faded
        strokeDashed
        stroke "black"
    ".active" & do
        opacity 1
        "stroke-width" -: "2px"
        stroke "black"
    ".missing" & do
        strokeRed
        strokeDashed

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
    "#react-graph" ? do
        "width" -: "calc(100% - 40px)"
        height100
        overflow hidden
        margin0
        display inlineBlock
        position absolute
        textAlign $ alignSide sideCenter
        left (px 40)
    "#react-graphRootSVG" ? do
        width100
        height100
        stroke "black"
        "stroke-linecap" -: "square"
        "stroke-miterlimit" -: "10"
        "shape-rendering" -: "geometricPrecision"
    ".highlight-nodes" ? do
        backgroundColor grey
    ".graph-control-button" ? do
        textAlign $ alignSide sideCenter
        backgroundColor white
        border solid (px 1) purple10
        "border-radius" -: "6px"
        "outline" -: "none"
        fontSize (px 14)
        fontWeight bold
        fontColor purple10
        cursor pointer
        display inlineBlock
        width (px 25)
        height (px 25)
        position absolute
        ":hover" & do
            backgroundColor purple6
            fontColor white
        ":active" & do
            backgroundColor purple10
            fontColor white
        ":disabled" & do
            border solid (px 1) grey6
            backgroundColor grey2
            fontColor grey6
    "#zoom-in-button" ? do
        top (px 85)
        right (px 30)
    "#zoom-out-button" ? do
        top (px 85)
        right (px 57)
    "#pan-up-button" ? do
        top (px 4)
        right (px 43)
    "#pan-down-button" ? do
        top (px 57)
        right (px 43)
    "#pan-right-button" ? do
        top (px 30)
        right (px 30)
    "#pan-left-button" ? do
        top (px 30)
        right (px 57)
    "#reset-button" ? do
        width (px 52)
        top (px 111)
        right (px 30)

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
        backgroundColor purple7
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
        "height" -: "calc(100% - 100px)"
        width100
        position relative
    "#sidebar" ? do
        display inlineBlock
        width (px 40)
        height100
        float floatLeft
        backgroundColor purple8
        position absolute
        paddingLeft (px 23)
    "#sidebar-button" ? do
        cursor pointer
        display inlineBlock
        width (px 40)
        height100
        float floatLeft
        backgroundColor purple10
        position absolute
        border solid (px 1) black
        ":hover" & do
            backgroundColor purple6
    "#sidebar-icon" ? do
        width (px 30)
        height (px 50)
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
        backgroundColor purple6
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
                    "background-color" -: "#5C497E !important"
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
        height (pct 86)
        display none
        overflowY scroll
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
        backgroundColor purple6
        fontSize (px 20)
        border solid (px 1) black
        textAlign $ alignSide sideCenter
        width (pct 90)
    ".spotlight" & do
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
        backgroundColor purple8
    ".disabled" & do
        backgroundColor grey2
        pointerEvents none
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
    ".flip" & do
        transform $ scaleX (-1)


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
    ".region-label" ? do
        fontSize (pt regionFontSize)
    ".region" ? do
        "fill-opacity" -: "0.25"
