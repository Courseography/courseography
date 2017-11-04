{-|
    Module      : Css.Post
    Description : Defines the CSS for the post page.

The post page is navigated to by clicking Check My POSt located at the top of
any Courseography page.
-}
module Css.Post
    (postStyles) where

import Clay
import Prelude hiding ((**))
import Css.Constants

-- |Defines the CSS for the post page.
postStyles :: Css
postStyles = do
    body ?
        do color grey1
           fontWeight normal
    tabsCSS
    postCSS

tabsCSS :: Css
tabsCSS = do
    "#posts" & do
        fontFamily ["Work Sans"][sansSerif]
        fontSize (px 15)
        -- width (pct 97)
        backgroundColor white
        -- border solid (px 1) grey2
        "border-radius" -: "4px"
        -- "box-shadow" -: "0 2px 2px -1px rgba(0, 0, 0, 0.055)"
        display block
        overflow hidden
        -- margin (px 8) (px 22) (px 8) (px 22)
        margin (px 15) (px 0) (px 25) (px 0)
        ul ? do
            width $ (pct 100)
            margin0
            li ? do
                "list-style-type" -: "none"
                display inlineBlock
                width (pct 32)
                --textAlign $ alignSide sideCenter
                "-webkit-transition" -: "all 0.2s"
                "-moz-transition" -: "all 0.2s"
                "-ms-transition" -: "all 0.2s"
                "-o-transition" -: "all 0.2s"
                "transition" -: "all 0.2s"
                ":hover" & do
                    "background-color" -: "#eeeeee !important"
                    -- a ? do
                    --     "color" -: "white !important"
                    --     cursor pointer
                a ? do
                    color black
                    display inlineBlock
                    lineHeight (px 50)
                    paddingLeft (px 24)
                    width (pct 70)
                    textDecoration none
            ".nav_selected" ? do
                border solid (px 1.5) grey5
                borderBottom solid (px 1.5) white
                -- backgroundColor grey6
            ".nav_not_selected" ? do
                borderBottom solid (px 1.5) grey5
                -- backgroundColor white
            ".credits_completed" ? do
                color mGreen
                fontSize (px 15)
            ".credits_not_completed" ? do
                color mRed
                fontSize (px 15)

postCSS :: Css
postCSS = do
    ".year_name" ? do
        fontFamily ["Work Sans"][sansSerif]
        fontSize (px 25)
        "font-style" -: "normal"
        borderBottom solid (px 2) darkRose
    ".year_course_list" ? do
        fontFamily ["Work Sans"][sansSerif]
        fontSize (px 18)
        marginTop (px 20)
        paddingLeft (px 5)
        fontStyle normal
        -- "font-style" -: "normal"
        "list-style-type" -: "none"
        ".code" ? do
            padding0
            "text-indent" -: "-20px;"
    "input" ? do
        fontSize (px 14)
    "#button_wrapper" ? do
        textAlign $ alignSide sideCenter
        height (px 50)
        paddingLeft (px 20)
    "#update" ? do
        height (px 40)
        fontSize (px 14)
        cursor pointer
        semiVisible
        ":hover" & do
            fullyVisible
    ".code:before" ? do
            "content" -: "'✔'"
            color white
    ".selected" ? do
        ".code" ? do
            color softGreen
            -- backgroundColor green2
            -- fontWeight bold
        ".code:before" ? do
            "content" -: "'✔'"
            color softGreen
        ".full_name" ? do
            backgroundColor green1
    "div" ? do
        ".code" ? do
            -- fontFamily ["Work Sans"][sansSerif]
            -- "font-weight" -: "300"
        --     backgroundColor beige1
        --     fontFamily ["HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", "Lucida Grande"][sansSerif]
            -- fontSize (px 18)
            paddingLeft (px 20)
        --     "box-shadow" -: "0 2px 2px -1px rgba(0, 0, 0, 0.055)"
        --     borderBottom solid (px 1) grey5
        --     lineHeight (px 50)
            "list-style-type" -: "none"
        --     textAlign $ alignSide sideCenter
        --     margin0
            width (pct 99)
        --     "margin-right" -: "10px"
            "-webkit-transition" -: "all 0.2s"
            "-moz-transition" -: "all 0.2s"
            "-ms-transition" -: "all 0.2s"
            "-o-transition" -: "all 0.2s"
            "transition" -: "all 0.2s"
            ".courseName" ? do
                -- fontFamily ["Work Sans"][sansSerif]
                -- "font-weight" -: "300"
                -- display inlineBlock
                -- fontFamily ["HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", "Lucida Grande"][sansSerif]
                -- lineHeight (px 50)
                paddingLeft (px 5)
                paddingRight (px 5)
                cursor pointer
                ":hover" & do
                    fontWeight bold
    i ? do
        color red
    "#post_specialist, #post_major, #post_minor" ? do
        fontFamily ["Work Sans"][sansSerif]
        "font-weight" -: "300"
        fontSize (px 18)
        position absolute
        -- paddingTop (px 30)
        -- paddingBottom (px 30)
        -- paddingLeft (px 50)
        -- paddingRight (px 50)
        padding (px 30) (px 50) (px 30) (px 50)
        height (pct 70)
        -- marginLeft (px 25)
        width (pct 97)
    "#spec_creds, #maj_creds, #min_creds" ? do
        display inlineBlock
        marginLeft nil
    ".more-info" ? do
        cursor pointer
        border solid (px 2) grey3
        "border-radius" -: "4px"
        backgroundColor grey4
        "box-shadow" -: "0 2px 2px -1px rgba(0, 0, 0, 0.055)"
        lineHeight (px 40)
        "list-style-type" -: "none"
        textAlign $ alignSide sideCenter
        margin0
        display none
        "margin-right" -: "10px"
        "-webkit-transition" -: "all 0.2s"
        "-moz-transition" -: "all 0.2s"
        "-ms-transition" -: "all 0.2s"
        "-o-transition" -: "all 0.2s"
        "transition" -: "all 0.2s"
    ".info_opened > div" ? do
        display block
    ".full_name" ? do
        paddingLeft (px 20)
        textAlign $ alignSide sideCenter
        margin0
    "input" ? do
        textAlign $ alignSide sideCenter
        height $ (px 40)
    "#notes" ? do
        "clear" -: "left"
        "float" -: "center"
        fontWeight bold
        paddingTop (px 20)
        -- textAlign $ alignSide sideCenter
        "> ul" ? do
            "list-style-type" -: "none"
    ".valid_extra_course" ? do
        color green
    ".not_valid_extra_course" ? do
        color red
    ".post_selected" ? do
        display block
    ".post_not_selected" ? do
        display none
