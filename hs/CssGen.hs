{-# LANGUAGE OverloadedStrings #-}

module CssGen where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Data.Text.Lazy
import System.Directory
import Consts
import CommonCssGen as COMMON

styleFiles :: [(String, Css)]
styleFiles = [
    ("../style/common/common.css", COMMON.common),
    ("../style/graph/graph_styles.css", graphStyles),
    ("../style/grid/timetable_styles.css", timetableStyles),
    ("../style/common/about.css", aboutStyles)
    ]

renderStyleFile :: (String, Css) -> IO ()
renderStyleFile (path, css) = writeFile path $ unpack $ render css

generateCSS :: IO ()
generateCSS = do
    createDirectoryIfMissing True "../style/common"
    createDirectoryIfMissing True "../style/graph"
    createDirectoryIfMissing True "../style/grid"
    Prelude.foldl1 (>>) $ Prelude.map renderStyleFile styleFiles


-- Style for graph

graphStyles = do
    graphContainer
    nodeCSS
    pathCSS
    resetCSS
    titleCSS
    modalCSS
    regionCSS
    regionShapeCSS

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
        "dominant-baseline" -: "central"
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
        -- Since groups are missing right now
        "rect" <? do
            stroke "black"
            fill systemsDark
    ".hybrid" & do
        cursor cursorDefault
        "rect" <? do
            fill "grey"
        "text" <? do
            fontSize (em 0.45)
            fill "white"
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
            fontSize (em 0.45)
            fontFamily ["Comic Sans MS"] [sansSerif]
            fontWeight bold
    -- TODO: get rid of this style
    "ellipse" ? do
        fill "none"
        stroke "black"
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
        "stroke-width" -: "2px"
    "data-active" @= "missing" & do
        faded
        strokeRed
        strokeDashed

resetCSS = "#resetButton" ? do
    fill "#990000"
    cursor pointer
    wideStroke
    "stroke" -: "#404040"
    "text" <? do
        fill "white"

graphContainer = do
    "#graph" ? do
        width (px 1100)
        minHeight (px 700)
        height (px 700)
        overflow hidden
        margin nil auto nil auto
        clear both
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


regionCSS = ".region-label" ? do
    fontSize (em 0.65)
    "text-anchor" -: "start"

regionShapeCSS = ".region" ? do
    faded

modalCSS = do
    ".ui-dialog" ? do
        outline solid (px 0) black
    ".ui-widget-overlay" ? do
        height100
        width100
        position fixed
        left nil
        top nil
    ".modal" ? do
        backgroundColor modalColor
        padding (px 20) (px 20) (px 20) (px 20)
        width (pct 70)
        height (pct 70)
        overflow auto
        position static
        p ? do
            color white
    ".ui-dialog-titlebar" ? do
        backgroundColor $ parse "#222266"
        color white
        fontSize (em 2)
        cursor move
        alignCenter
    ".ui-dialog-titlebar-close" ? do
        display none
    "#bottom-content-container" ? do
        paddingTop (em 1)
    ".ui-width-overlay" ? do
        height100
        width100
        left nil
        position fixed
        top nil
    ".ui-dialog" ? do
        tr ? do
            margin nil auto nil auto


-- Timetable styles
timetableStyles = do
    body ? do
        overflowX hidden
    ".main" ? do -- TODO: change to id, and pick better name
        height (pct 84)
        margin0
    searchCSS
    timetableCSS
    courseSelectCSS
    tdColours
    --infoCSS
    conflictNotificationStyle

searchCSS = do
    "#search-layout" ? do
        backgroundColor purple1
        margin0
        padding0
        overflowY hidden
        overflowX hidden
    "#filter-container" ? do
        padding (px 10) (px 10) (px 10) (px 10)
        height (pct 10)
        form <? do
            margin0
    "#search-container" ? do
        alignCenter
        color white
        height (pct 90)
        width100
        margin0
        padding (em 0.5) 0 (em 1) 0
        overflowY auto
    "#search-list" ? do
        margin0
        padding nil nil nil nil
        height100
        width100
        ul <? do
            margin0
            padding0
            li <? do
                display block
                margin0
                padding0
                ":hover" & do
                    fontWeight bold
                    cursor pointer
                    textDecoration underline
            ".starred-course" & do
                backgroundColor blue1

timetableCSS = do
    ".timetable-container" ? do
        backgroundColor white -- purple2
        padding0
        margin0
    ".timetable" ? do
        margin0
        padding0
        "table-layout" -: "fixed"
        "border-collapse" -: "collapse"
        caption <? do
            fontSize (em 1.6)
            margin (em 0.5) 0 (em 0.5) 0
            alignCenter
        "#timetable-F" & do
            borderRight solid (px 2) black
            ".timetable-time" ? do
                textAlign $ alignSide sideRight
                paddingRight (px 10) -- important
        "#timetable-S" & do
            borderLeft solid (px 2) black
            ".timetable-time" ? do
                textAlign $ alignSide sideLeft
                paddingLeft (px 10) -- important
        tbody |> tr |> td <> thead |> th ? do
            width (pct 13.5)
            height (px 35)
            padding0 -- !important
            margin0 -- !important
            alignCenter
            "vertical-align" -: "middle"
            overflow hidden
            borderColor pink1 -- !important
            lineHeight (em 0.9)
        th ? do
            fontSize (em 1.1)
            fontWeight normal
            backgroundColor $ rgba 219 148 184 1
            borderBottom solid (px 2) pink1 -- important
            ".term-name" & do
                padding0
                width (pct 10)
                fontWeight bold
        td ? do
            fontSize (em 0.9)
        ".timetable-time" ? do
            width (pct 12)
    -- Overriding bootstrap
    ".col-md-2" ? do
        width (pct 14)
    ".col-md-8" ? do
        width (pct 72)
    ".col-md-pull-2" ? do
        right (pct 14)
    ".col-md-push-8" ? do
        left (pct 72)


conflictNotificationStyle = "#dialog" ? do
    position fixed
    backgroundColor white
    width100
    display none
    visibility hidden
    alignCenter
    borderBottom solid (px 2) $ parse "#330000"
    "z-index" -: "2147483647"

courseSelectCSS = do
    "#course-select-wrapper" ? do
        margin0
        padding0
        height100
        overflow hidden
        backgroundColor purple1
        color white
    "#course-select" ? do
        padding0
        margin (px 18) 0 0 (px 17)
        width100
        height100
        alignCenter
        overflowY scroll
        overflowX hidden
        "list-style-type" -: "none"
        ".ui-accordion-header" ? do
            outline solid nil white
        li <? do
            width (pct 95)
            clear both
            h3 ? do
                cursor pointer
                margin0
                padding (em 0.25) 0 (em 0.25) 0
                display block
                width100
                minHeight (px 40)
                borderTop solid (px 1) black
                "#clear-all" <? do
                    h3 <? do
                        margin0
                        padding (em 0.25) (em 0.25) (em 0.25) (em 0.25)
                        cursor pointer
                        ":hover" & do
                            backgroundColor blue1
                        ".ui-accordion-header-active" & do
                            backgroundColor blue1 -- important
                "satisfied" *= "false" & do
                    "taken" *= "true" & do
                        backgroundColor blue3
                    backgroundColor red3
                "satisfied" *= "true" & do
                    "taken" *= "true" & do
                        backgroundColor blue4
        ".close-icon" ? do
            width (px 18)
            height (px 20)
            padding (px 1) 0 (px 1) 0
            cursor pointer
            opacity 0.8
            ":hover" & do
                opacity 1.0
        ".icon-div" ? do
            width (px 20)
            minHeight (px 40)
            float floatLeft
        ".sections" ? do
            -- overflow: auto <-- really necessary?
            cursor pointer -- necessary?
            "ul" <? do
                display block
                margin0
                padding0
                "list-style-type" -: "none"
                ".sectionList-F" & do
                    width (pct 50)
                    float floatLeft
                    borderRight solid (px 1) black
                ".sectionList-S" & do
                    width (pct 50)
                    float floatRight
                    borderLeft solid (px 1) black
                li <? do
                    fontSize (em 0.8)
                    backgroundColor blue2
                    ":hover" & do
                        backgroundColor blue3
                    "clicked" *= "true" & do
                        "satisfied" *= "false" & do
                            backgroundColor red3
                            ":hover" & do
                                backgroundColor red4
                        "satisfied" *= "true" & do
                            backgroundColor blue3

tdColours = ".timetable " ?  do
    td ? do
        ".hover-time" & do
            backgroundColor blue4 -- important
        "hover" *= "good" & do
            backgroundColor blue3
        "hover" *= "conflict" & do
            backgroundColor red1
        "hover" *= "remove" & do
            opacity 0.5
            transition "all" (sec 0.5) easeInOut (sec 0)
        "in-conflict" *= "true" & do
            backgroundColor red1
            ":hover" & do
                backgroundColor red2
        "satisfied" *= "false" & do
            backgroundColor red3
            ":hover" & do
                backgroundColor red4
    td # ("in-conflict" *= "false") # ("satisfied" *= "true") ? do
        backgroundColor blue3
        Clay.empty & do
            backgroundColor white
            
-- info layout
infoCSS = "#info-layout" ? do
    padding0
    margin0
    width100
    height (em 7)
    alignCenter
    h2 <? do
        fontSize (em 1.3)
        margin (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        fontWeight bold
    h4 <? do
        fontSize (em 1.15)
        margin (em 0.5) (em 0.5) (em 0.5) (em 0.5)

-- About page
aboutStyles = "#aboutDiv" ? do
    maxWidth (px 1000)
    padding 0 (em 1) 0 (em 1)
    margin nil auto nil auto
    textAlign justify
    h1 <> h2 <> h3 <? do
        color blue3

-- Currently not used

fceCountCSS = "#FCECountDiv" ? do
    backgroundColor fceCountColor
    float floatRight
    padding0
    lineHeight (px 40)
    "vertical-align" -: "middle"
    fontSize (em 1.35)
    alignCenter

