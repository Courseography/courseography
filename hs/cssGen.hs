{-# LANGUAGE OverloadedStrings #-}

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Data.Text.Lazy

styleFiles :: [(String, Css)]
styleFiles = [
    ("../style/common/common.css", common),
    ("../style/graph/graph_styles.css", graphStyles),
    ("../style/grid/timetable_styles.css", timetableStyles),
    ("../style/common/about.css", aboutStyles)
    ]

renderStyleFile :: (String, Css) -> IO ()
renderStyleFile (path, css) = writeFile path $ unpack $ render css

main = Prelude.foldl1 (>>) $ Prelude.map renderStyleFile styleFiles

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
    disclaimerCSS

headerCSS = do
    ".header" ?
        do margin0
           padding 0 (em 0.5) 0 (em 0.5)
           backgroundColor $ blue1
           color white
           h2 ?
             do fontSize $ em 1.6
                fontWeight bold
                textAlign $ alignSide sideLeft
                width $ px 200
                display inlineBlock
                margin (px 10) 0 (px 5) 0
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
        alignCenter
    h2 ? do
        alignCenter
    h3 ? do
        alignCenter
    h4 ? do
        width100
        fontSize $ em 1.1
        alignCenter
        margin (em 0.3) 0 (em 0.3) 0

-- Disclaimer
disclaimerCSS = "#disclaimerDiv" ? do
    padding 0 (em 1) (em 0.5) (em 1)
    fontSize (pt 11)


-- Style for graph

graphStyles = do
    graphContainer
    nodeCSS
    pathCSS
    resetCSS
    titleCSS
    modalCSS

alignCenter = textAlign $ alignSide sideCenter
cursor = (-:) "cursor"
stroke = (-:) "stroke"
fill = (-:) "fill"

wideStroke = "stroke-width" -: "3"
faded = opacity 0.4
semiVisible = opacity 0.7
strokeRed = "stroke" -: "#CC0011"
strokeDashed = do
    "stroke-dasharray" -: "8,5"
    "stroke-width" -: "2px"

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
        "dominant-baseline" -: "central"
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
        -- Since groups are missing right now
        "rect" <? do
            stroke "black"
            fill systemsDark
    ".hybrid" & do
        cursor "default"
        "rect" <? do
            fill "grey"
        "text" <? do
            fontSize (em 0.45)
            fill "white"
    ".bool" & do
        cursor "default"
        "ellipse" <? do
            fill "none"
            stroke "black"
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
        width (px 1100)
        minHeight (px 700)
        height (px 700)
        overflow hidden
        margin 0 auto 0 auto
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


-- Course Modal
modalColor = parse "#374AA1"

modalCSS = do
    ".ui-dialog" ? do
        outline solid (px 0) black
    ".ui-widget-overlay" ? do
        height100
        width100
        position fixed
        left 0
        top 0
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
        "cursor" -: "move"
        alignCenter
    ".ui-dialog-titlebar-close" ? do
        display none
    "#bottom-content-container" ? do
        paddingTop (em 1)
    ".ui-width-overlay" ? do
        height100
        width100
        left 0
        position fixed
        top 0
    ".ui-dialog" ? do
        tr ? do
            margin 0 auto 0 auto


-- Timetable styles
timetableStyles = do
    body ? do
        overflowX hidden
    ".main" ? do -- TODO: change to id, and pick better name
        "height" -: "calc(90% - 3.55em)"
        margin0
    searchCSS
    timetableCSS
    courseSelectCSS
    tdColours
    infoCSS
    conflictNotificationStyle


searchCSS = do
    "#search-layout" ? do
        backgroundColor purple1
        height100
        margin0
        padding0
        overflowY scroll
        overflowX hidden
    "#filter-container" ? do
        padding (px 10) (px 10) (px 10) (px 10)
        form <? do
            margin0
    "#search-container" ? do
        alignCenter
        color white
        height100
        width100
        margin0
        padding (em 0.5) 0 (em 1) 0
    "#search-list" ? do
        margin0
        padding 0 0 0 0
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
                    cursor "pointer"
                    textDecoration underline
            ".starred-course" & do
                backgroundColor blue1

purple1 = parse "#46364A"
purple2 = parse "#7E4D66"
pink1 = parse "#DB94B8"

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
        td <> th ? do
            width (pct 18)
            height (px 35)
            padding0 -- !important
            margin0 -- !important
            alignCenter
            "vertical-align" -: "middle"
            overflow hidden
            borderColor pink1 -- !important
        th ? do
            fontSize (em 1.1)
            fontWeight normal
            backgroundColor $ rgba 219 148 184 1
            borderBottom solid (px 2) pink1 -- important
            ".term-name" & do
                padding0
                width (pct 10)
                fontWeight bold
        ".timetable-time" ? do
            width (pct 10)
            borderNone
            padding (px 10) (px 10) (px 10) (px 10)


conflictNotificationStyle = "#dialog" ? do
    position fixed
    backgroundColor white
    width100
    display none
    visibility hidden
    alignCenter
    borderBottom solid (px 2) $ parse "#330000"
    "z-index" -: "2147483647"


borderNone = border solid (px 0) white

courseSelectCSS = do
    "#course-select-wrapper" ? do
        margin0
        padding0
        overflow hidden
        height100
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
        li <? do
            width (pct 95)
            clear both
            h3 <? do
                "cursor" -: "pointer"
                margin0
                padding (em 0.25) 0 (em 0.25) 0
                display block
                width100
                outline solid (px 0) white
                borderTop solid (px 1) black
                "#clear-all" <? do
                    h3 <? do
                        margin0
                        padding (em 0.25) (em 0.25) (em 0.25) (em 0.25)
                        "cursor" -: "pointer"
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
            width (px 20)
            height (px 20)
            float floatLeft
            padding (px 7) 0 0 (px 5)
        ".sections" ? do
            -- overflow: auto <-- really necessary?
            "cursor" -: "pointer" -- necessary?
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
    td # ("in-conflict" *= "false") # ("satisfied" *= "true") # ("type" *= "L") ? do
            backgroundColor blue3
    td # ("in-conflict" *= "false") # ("satisfied" *= "true") # ("type" *= "T") ? do
            backgroundColor teal1
    td # ("in-conflict" *= "false") # ("satisfied" *= "true") # ("type" *= "P") ? do
            backgroundColor orange1


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
    margin 0 auto 0 auto
    textAlign justify
    h1 <> h2 <> h3 <? do
        color blue3


-- Currently not used

-- FCECount
fceCountColor = parse "#66C2FF"

fceCountCSS = "#FCECountDiv" ? do
    backgroundColor fceCountColor
    float floatRight
    padding0
    lineHeight (px 40)
    "vertical-align" -: "middle"
    fontSize (em 1.35)
    alignCenter

