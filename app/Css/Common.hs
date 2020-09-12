{-|
    Module      : Css.Common
    Description : Defines the CSS present in every page.

The CSS present in every page includes the header, common CSS, disclaimer at
the bottom, and (oddly enough) the modal that appears when nodes in the
graph are clicked.
-}
module Css.Common
    (common) where

import Clay
import Prelude hiding ((**))
import Css.Constants

-- |Defines CSS common to all pages.
common :: Css
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
           height100
           minHeight $ pct 100
           fontSize $ pt 17
           fontFamily ["Trebuchet MS", "Arial"] [sansSerif]
    headerCSS
    aDefaultCSS
    disclaimerCSS
    modalCSS

{- headerCSS
 - Generates the CSS for the header located at the top
 - of every page -}
headerCSS :: Css
headerCSS = do
    ".header" ?
        do margin0
           padding 0 (em 0.5) 0 (em 0.5)
           backgroundColor purple10
           border solid (px 1) black
           color white
           img ?
             do display inlineBlock
                margin 0 0 (px 5) 0
           height (px 50)
    "#nav-links" ? do
        "list-style" -: "none"
        minWidth (px 687)
        paddingTop (px 10)
        margin nil nil nil nil
        display inlineBlock
        a ?
          fontWeight normal
        li <? do
            textAlign $ alignSide sideCenter
            display inlineBlock
            padding 0 (px 10) 0 (px 10)
            a <? do
                color white
                "text-shadow" -: "0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000, 0 0 2px #000;"
                hover &
                    color darkgray
        height (px 50)
        position absolute
    "#nav-fb" ? do
        float floatRight
        height (px 50)
        "#nav-fb-post" ? do
            backgroundColor blueFb
            height (px 40)
            margin (px 5) (px 10) (px 5) (px 10)
            padding nil (px 5) nil (px 5)
            lineHeight (px 40)
            verticalAlign middle
            display none
            a <? do
                color white
            borderRadius (px 3) (px 3) (px 3) (px 3)
            cursor pointer
        ".fb-login-button" ? do
            height (px 40)
            verticalAlign middle
            width (px 140)
            overflow hidden
            margin (px 5) nil (px 5) nil
    "#courseography-header" ? do
        width (px 280)
        height (px 50)
        paddingBottom (px 3)
    "#nav-export" ? do
        cursor pointer

{- aDefaultCSS
 - Generates default CSS. -}
aDefaultCSS :: Css
aDefaultCSS = do
    a <> a # hover <> a # visited <> a # active ? do
        fontWeight bold
        textDecoration none
        color $ parse "#4C004C"

{- disclaimerCSS
 - Generates CSS for the disclaimer located at the foot
 - of all pages. -}
disclaimerCSS :: Css
disclaimerCSS = "#disclaimerDiv" ? do
    padding 0 (em 1) 0 (em 1)
    fontSize (pt 11)

{- modalCSS
 - Generates CSS for the modal that appears
 - when nodes in the graph are clicked. -}
modalCSS :: Css
modalCSS = do
    ".ui-dialog" ? do
        outline solid (px 0) black
        color black
        overflowX hidden
        overflowY auto
        backgroundColor white
        boxShadow . pure $ shadowWithBlur (px 8) (px 8) (px 8)
    ".ui-widget-overlay" ? do
        height100
        width100
        position fixed
        left nil
        top nil
    ".modal-header" ? do
        color blue3
    ".modal-body" ? do
        p ? do
            fontSize (pt 12)
            margin (pt 5) 0 (pt 5) 0
            lineHeight (em 1.3)
            textAlign $ alignSide sideLeft
    ".leaflet-container" ? do
        height100
        width100
    "#zoom-button" ? do
        borderRadius (px 7) (px 7) (px 7) (px 7)
        marginTop (px 10)
        color "#7a6a96c7"
        backgroundColor "#dedae5"
        border solid (px 2) "#7a6a96c7"
    "#zoom-button:hover" ? do
        color "#dedae5"
        backgroundColor "#7a6a96c7"
    "#map-sidebar" ? do
        padding0
        overflowY auto
    ".accordian-collapsed" ? do
        borderRadius (px 7) (px 7) (px 7) (px 7)
        marginBottom (px 10)
    ".accordian-expanded" ? do
        borderRadius (px 7) (px 7) (px 0) (px 0)
    ".sidebar-day" ? do
        cursor pointer
        fontWeight bold
        padding (px 10) (px 10) (px 10) (px 10)
        fontFamily ["Work Sans"] [sansSerif]
        "#map-day-title" ? do
            width $ pct 100 @-@ (px 30)
            height (px 40)
            position relative
            display inlineBlock
        "#expand-icon" ? do
            height (px 30)
            width (px 30)
    ".selected-day" ? do
        color "#dedae5"
        backgroundColor purple8
    ".unselected-day" ? do
        color "#7a6a96c7"
        backgroundColor "#dedae5"
    "#map-day-list" ? do
        "list-style" -: "none"
        paddingLeft (px 0)
        ".map-day-lec" ? do
            paddingLeft (px 10)
            paddingRight (px 10)
            fontFamily ["Work Sans"] [sansSerif]
            marginTop (px (-3))
        ".unselected-day-lec" ? do
            border solid (px 3) "#dedae5"
        ".selected-day-lec" ? do
            border solid (px 3) purple8
            position relative
    ".modal-class" ? do
        overflowY auto
        position absolute
        top (px 100)
        left (px 300)
        right (px 300)
        bottom (px 150)
        borderRadius (px 10) (px 10) (px 10) (px 10)
        backgroundColor white
        boxShadow . pure $ shadowWithBlur (px 0) (px 0) (px 30)
    ".overlay" ? do
        position fixed
        left nil
        right nil
        top nil
        bottom nil
        backgroundColor (setA 150 black)
    ".ui-dialog-titlebar" ? do
        color blue3
        cursor move
        paddingLeft (px 25)
        height (em 1.8)
        lineHeight (em 1.8)
        fontSize (em 1)
        borderBottom solid (px 1) black
    ".ui-dialog-titlebar-close" ? do
        display none
    ".ui-width-overlay" ? do
        height100
        width100
        left nil
        position fixed
        top nil
    ".ui-dialog" ? do
        tr ? do
            margin nil auto nil auto
    "#course-video-div" ? do
        margin (pt 5) 0 (pt 5) 0
        width100
        height100
    "#course-video" ? do
        width100
        height100
    -- new modal CSS, need to remove above later
    ".modal-header" ? do
        color blue3
        padding0
        paddingLeft (px 25)
        lineHeight (em 1.8)
        fontSize (em 1)
        borderBottom solid (px 1) black
        textAlign $ alignSide sideLeft
    fbModalCSS



fbModalCSS :: Css
fbModalCSS = do
    "#post-image" ? do
        border solid (px 3) black
        borderRadius (px 5) (px 5) (px 5) (px 5)
        margin nil auto (px 10) auto
        maxWidth (pct 100)
    "#modal-buttons" ? do
        float floatRight
        margin (px 20) auto (px 20) auto
        ".btn" ? do
            margin nil (px 5) nil (px 5)
