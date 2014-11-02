{-# LANGUAGE OverloadedStrings #-}

import Clay
import Prelude hiding ((**))
import Data.Monoid

main = putCss graphStyles

margin0 = margin 0 0 0 0
padding0 = padding 0 0 0 0

width100 = width $ pct 100
height100 = height $ pct 100

common = do 
    html ? 
        do margin0
           padding0
           width100
           height100
           overflowY auto
    body ?
        do margin0
           padding0
           width100
           minHeight $ pct 100
           fontSize $ pt 16
           fontFamily ["Trebuchet MS", "Arial"] [sansSerif]
    headerCSS
    aDefaultCSS

headerCSS = do
    ".header" ?
        do margin0
           padding 0 (em 0.5) 0 (em 0.5)
           backgroundColor $ parse "#261B2A"
           color white
           h2 ? 
             do fontSize $ em 1.6
                fontWeight bold
                textAlign $ alignSide sideLeft
                width $ px 200
                display inlineBlock
    "#nav-links" ?
        do 
            "list-style" -: "none"
            width $ pct 70
            margin 0 0 0 0
            display inlineBlock
            li <? do
                textAlign $ alignSide sideCenter
                display inlineBlock
                padding 0 (px 10) 0 (px 10)
                a <? do
                    color white

aDefaultCSS = do
    a <> a # hover <> a # visited <> a # active ? do
        fontWeight bold
        textDecoration none
        color $ parse "#4C004C"


headers = do
    h1 ? do
        textAlign $ alignSide sideCenter
    h2 ? do
        textAlign $ alignSide sideCenter
    h3 ? do
        textAlign $ alignSide sideCenter
    h4 ? do
        width100
        fontSize $ em 1.1
        alignCenter
        margin (em 0.3) 0 (em 0.3) 0


-- Style for graph

graphStyles = do
    graphContainer
    nodeCSS
    pathCSS
    resetCSS
    titleCSS

alignCenter = textAlign $ alignSide sideCenter
cursor = (-:) "cursor"
stroke = (-:) "stroke"
fill = (-:) "fill"

wideStroke = "stroke-width" -: "3"
faded = opacity 0.4
semiVisible = opacity 0.7
strokeRed = "stroke" -: "#CC0011"
strokeDashed = "stroked-dasharray" -: "8,8"

theoryDark = "#B1C8D1"
coreDark = "#C9C9C9"
seDark = "#E68080"
systemsDark = "#C285FF"
graphicsDark = "#66A366"
dbwebDark = "#FFE680"
numDark = "#DBB8FF"
aiDark = "#80B2FF"
hciDark = "#B8FF70"
titleColour = "#072D68"

nodeCSS = "g" ? do
    "text" ? do
        alignCenter
        "stroke" -: "none"
        userSelect none
        "-webkit-touch-callout" -: "none"
        "-webkit-user-select" -: "none"
        "-khtml-user-select" -: "none"
        "-moz-user-select" -: "none"
        "-ms-user-select" -: "none"
        "text-anchor" -: "middle"
    ".node" & do
        cursor "pointer"
        "text" ? do
            fontSize (pt 12)
        "data-active" @= "active" & do
            "rect" <? do
                wideStroke
        "data-active" @= "overridden" & do
            "rect" <? do
                wideStroke
                strokeRed
        "data-active" @= "inactive" & do
            "rect" <? do
                faded
                strokeDashed
        "data-active" @= "takeable" & do
            "rect" <? do
                semiVisible
        "data-active" @= "missing" & do
            "rect" <> "ellipse" <? do
                wideStroke
                strokeRed
        "data-active" @= "unlit" & do
            wideStroke
            strokeRed
        "data-group" @= "theory" & do
            "rect" <? do
                fill theoryDark
        "data-group" @= "core" & do
            "rect" <? do
                fill coreDark
        "data-group" @= "se" & do
            "rect" <? do
                fill seDark
        "data-group" @= "systems" & do
            "rect" <? do
                fill systemsDark
        "data-group" @= "graphics" & do
            "rect" <? do
                fill graphicsDark
        "data-group" @= "dbweb" & do
            "rect" <? do
                fill dbwebDark
        "data-group" @= "num" & do
            "rect" <? do
                fill numDark
        "data-group" @= "ai" & do
            "rect" <? do
                fill aiDark
        "data-group" @= "hci" & do
            "rect" <? do
                fill hciDark
    ".hybrid" & do
        cursor "default"
        "rect" <? do
            fill "grey"
        "text" <? do
            fontSize (em 0.6)
            fill "white"
    ".bool" & do
        cursor "default"
        "ellipse" <? do
            fill "none"
        "text" <? do
            fontSize (em 0.5)
            fontFamily ["Comic Sans MS"] [sansSerif]
            fontWeight bold
    ".spotlight" & do
        semiVisible
        fill "white"
        stroke "none"

pathCSS = "path" ? do
    fill "none"
    "data-active" @= "takeable" & do
        strokeDashed
    "data-active" @= "inactive" & do
        faded
        strokeDashed
    "data-active" @= "active" & do
        opacity 1
    "data-active" @= "missing" & do
        faded
        strokeRed
    
resetCSS = "#resetButton" ? do
    fill "#990000"
    cursor "pointer"
    wideStroke
    "stroke" -: "#404040"
    "text" <? do
        fill "white"

graphContainer = do
    "#graph" ? do
        width (pct 95)
        minHeight (px 600)
        overflow hidden
        margin 0 auto 0 auto
        -- clear both
    "#graphRootSVG" ? do
        width100
        height100
        stroke "black"
        "stroke-linecap" -: "square"
        "stroke-miterlimit" -: "10"
        "shape-rendering" -: "geometricPrecision"


titleCSS = "#svgTitle" ? do
    fontSize $ em 2.5
    fontWeight bold
    fontFamily ["Bitter"] [serif]
    fontStyle italic
    fill titleColour
