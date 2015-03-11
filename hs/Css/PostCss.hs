{-# LANGUAGE OverloadedStrings #-}

module Css.PostCss where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

-- Post Styles
postStyles = do
    body ?
        do backgroundColor beige1
           color grey1
           fontWeight $ normal
    tabsCSS
    postCSS


tabsCSS = do
    "#posts" & do 
        fontFamily ["HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", "Lucida Grande"][sansSerif]
        fontSize $ (px 17)
        width $ pct 97
        backgroundColor white 
        border solid (px 1) grey2
        "border-radius" -: "4px"
        "box-shadow" -: "0 2px 2px -1px rgba(0, 0, 0, 0.055)"
        display block
        overflow hidden
        margin (px 8) (px 22) (px 8) (px 22)
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
                    "background-color" -: "#9C9C9C !important"
                    a ? do
                        "color" -: "white !important" 
                a ? do
                    color black
                    display inlineBlock
                    lineHeight (px 50)
                    paddingLeft (px 24)
                    width (pct 70)
                    textDecoration none

                
postCSS = do
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
    "div" ? do
        ".code" ? do
            fontFamily ["HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", "Lucida Grande"][sansSerif]
            fontSize (px 20)
            paddingLeft (px 20)
            "cursor" -: "pointer"
            "box-shadow" -: "0 2px 2px -1px rgba(0, 0, 0, 0.055)"
            borderBottom solid (px 1) grey5
            lineHeight (px 50)
            "list-style-type" -: "none"
            textAlign $ alignSide sideCenter
            margin0
            width (pct 97)
            "margin-right" -: "10px"
            "-webkit-transition" -: "all 0.2s"
            "-moz-transition" -: "all 0.2s"
            "-ms-transition" -: "all 0.2s"
            "-o-transition" -: "all 0.2s"
            "transition" -: "all 0.2s"
    i ? do
        color red
    "#div_specialist, #div_major, #div_minor" ? do
        position absolute
        "margin-above" -: "30px"
        paddingBottom (px 30)
        display none
        height (pct 70)
        marginLeft (px 25)
        width (pct 97)
    "#spec_creds, #maj_creds, #min_creds" ? do
        display inlineBlock
        marginLeft nil 
        color red
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
    ".full_name" ? do
        paddingLeft (px 20)
        textAlign $ alignSide sideCenter
        margin0
    "input" ? do
        textAlign $ alignSide sideCenter
        height $ (px 40)
    "#notes" ? do
        textAlign $ alignSide sideCenter
    
