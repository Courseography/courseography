{-|
    Module      : Css.Timetable
    Description : Defines the CSS for the timetable page.

The timetable page is found by navigating to the search button at the top of
Courseography.
-}
module Css.Timetable
    (timetableStyles) where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

-- |Defines the CSS for the timetable page.
timetableStyles :: Css
timetableStyles = do
    ".main" ? do -- TODO: change to id, and pick better name
        height (pct 84)
        margin0
    gridCSS
    searchCSS
    timetableCSS
    courseSelectCSS
    tdColours
    modalContainerCSS
    --infoCSS

gridCSS :: Css
gridCSS = do
    "#grid-body" ? do
        minHeight (px 540)

{- searchCss
 - Generates CSS for the search box on
 - the timetable page. -}
searchCSS :: Css
searchCSS = do
    "#search-layout" ? do
        backgroundColor purple8
        margin0
        padding0
        overflowY hidden
        overflowX hidden
    "#filter-container" ? do
        padding (px 10) (px 10) (px 10) (px 10)
        height (pct 10)
        form <? do
            background ((url "/static/res/ico/search.png", noRepeat), placed sideRight sideCenter)
            margin (em 1) 0 0 0
            paddingRight (px 34)
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

{- timetableCSS
 - Generates CSS for the timetable. -}
timetableCSS :: Css
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
                borderPink borderBottom
        "#timetable-S" & do
            borderLeft solid (px 2) black
            ".timetable-time" ? do
                textAlign $ alignSide sideLeft
                paddingLeft (px 10) -- important
                borderPink borderBottom
        tbody |> tr |> td <> thead |> tr |> th ? do
            width (pct 13.5)
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
            height (px 35)
        td ? do
            "rowspan" *= "1" & do
                fontSize (em 0)
            "rowspan" *= "2" & do
                fontSize (em 0.9)
            fontSize (em 0.5)
            height (px 18)
        ".timetable-time" ? do
            width (pct 12)
        ".timetable-dummy-cell" ? do
            width (pct 0.00000000001) -- arbitary size of as small as possible
            borderStyle none
        ".timetable-half-cell" ? do
            display none
        ".timetable-half-cell-display" ? do
            display tableCell
        ".timetable-cell" ? do
            borderPink borderBottom
            borderPink borderTop
            borderLeftStyle none
            borderRightStyle none
        ".timetable-cell-tophalf" ? do
            borderBottomStyle none
            borderPink borderTop
            borderLeftStyle none
            borderRightStyle none
        ".timetable-cell-bottomhalf" ? do
            borderPink borderBottom
            borderTopStyle none
            borderLeftStyle none
            borderRightStyle none
        ".timetable-edge" ? do
            borderPink borderTop
            borderBottomStyle none
            borderPink borderLeft
            borderPink borderRight
        ".timetable-middle" ? do
            borderBottomStyle none
            borderTopStyle none
            borderPink borderLeft
            borderPink borderRight

    -- Overriding bootstrap
    ".col-md-2" ? do
        width (pct 14)
    ".col-md-8" ? do
        width (pct 72)
    ".col-md-pull-2" ? do
        right (pct 14)
    ".col-md-push-8" ? do
        left (pct 72)

{- courseSelectCSS
 - Generates CSS for the course selection
 - sidebar. -}
courseSelectCSS :: Css
courseSelectCSS = do
    "#course-select-wrapper" ? do
        margin0
        padding0
        height100
        backgroundColor purple8
        color white
    "#course-select" ? do
        padding (px 18) 0 0 (px 17)
        margin0
        height100
        alignCenter
        overflow auto
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
                "data-satisfied" *= "false" & do
                    "taken" *= "true" & do
                        backgroundColor blue3
                    backgroundColor red3
                "data-satisfied" *= "true" & do
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
                        "data-satisfied" *= "false" & do
                            backgroundColor red3
                            ":hover" & do
                                backgroundColor red4
                        "data-satisfied" *= "true" & do
                            backgroundColor blue3

{- tdColours
 - Generates CSS to colour the timetable. -}
tdColours :: Css
tdColours = ".timetable " ?  do
    td ? do
        ".hover-time" & do
            backgroundColor blue4 -- important
        "hover" *= "good" & do
            backgroundColor blue3
        "hover" *= "remove" & do
            opacity 0.5
            transition "all" (sec 0.5) easeInOut (sec 0)
        "data-satisfied" *= "false" & do
            backgroundColor red3
            ":hover" & do
                backgroundColor red4
        "data-in-conflict" *= "true" & do
            backgroundColor red1
            ":hover" & do
                backgroundColor red2
    td # ("data-in-conflict" *= "false") # ("data-satisfied" *= "true") ? do
        backgroundColor blue3
        Clay.empty & do
            backgroundColor white
    td # ("hover" *= "conflict") # ("data-satisfied" *= "true") <>
        td # ("hover" *= "conflict") # ("data-satisfied" *= "false") ? do
        backgroundColor red1

modalContainerCSS :: Css
modalContainerCSS = do
    "#modal-content-container" ? do
        a <? do
            textDecoration none
            marginLeft (px 10)
            "outline" -: "none"
            ":hover" & do
                textDecoration underline
