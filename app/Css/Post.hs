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
        fontFamily ["Work Sans"] [sansSerif]
        fontSize (px 15)
        backgroundColor white
        "border-radius" -: "4px"
        display block
        overflow hidden
        margin (px 15) (px 0) (px 25) (px 0)
        ul ? do
            width $ (pct 100)
            margin0
            li ? do
                "list-style-type" -: "none"
                display inlineBlock
                width (pct 32)
                "-webkit-transition" -: "all 0.2s"
                "-moz-transition" -: "all 0.2s"
                "-ms-transition" -: "all 0.2s"
                "-o-transition" -: "all 0.2s"
                "transition" -: "all 0.2s"
                ":hover" & do
                    "background-color" -: "#eeeeee !important"
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
            ".nav_not_selected" ? do
                borderBottom solid (px 1.5) grey5
            ".credits_completed" ? do
                color mGreen
                fontSize (px 15)
            ".credits_not_completed" ? do
                color mRed
                fontSize (px 15)

postCSS :: Css
postCSS = do
    ".year_name" ? do
        fontFamily ["Work Sans"] [sansSerif]
        fontSize (px 25)
        "font-style" -: "normal"
        borderBottom solid (px 2) darkRose
    ".year_course_list" ? do
        fontFamily ["Work Sans"] [sansSerif]
        fontSize (px 18)
        marginTop (px 20)
        paddingLeft (px 5)
        fontStyle normal
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
            "content" -: "'\\2713'"
            color white
            paddingRight (px 5)
    ".selected" ? do
        ".code" ? do
            color softGreen
        ".code:before" ? do
            "content" -: "'\\2713'"
            color softGreen
        ".full_name" ? do
            backgroundColor green1
    "div" ? do
        ".code" ? do
            paddingLeft (px 20)
            "list-style-type" -: "none"
            width (pct 99)
            "-webkit-transition" -: "all 0.2s"
            "-moz-transition" -: "all 0.2s"
            "-ms-transition" -: "all 0.2s"
            "-o-transition" -: "all 0.2s"
            "transition" -: "all 0.2s"
            ".courseName" ? do
                cursor pointer
                ":hover" & do
                    fontWeight bold
    i ? do
        color red
    "#post_specialist, #post_major, #post_minor" ? do
        fontFamily ["Work Sans"] [sansSerif]
        "font-weight" -: "300"
        fontSize (px 18)
        position absolute
        padding (px 30) (px 50) (px 30) (px 50)
        height (pct 70)
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
    "div[id*='_category_']" ? do
        " .code:after" ? do
            "content" -: "'\\2B9F'"
            paddingLeft (px 10)
            cursor pointer
    "div[id*='_inq']" ? do
        " .code:after" ? do
            "content" -: "'\\2B9F'"
            paddingLeft (px 10)
            cursor pointer
