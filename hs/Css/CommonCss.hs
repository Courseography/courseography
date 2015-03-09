{-# LANGUAGE OverloadedStrings #-}

module Css.CommonCss where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

{- common
 - Generates CSS common to all pages. -}

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

{- headerCSS
 - Generates the CSS for the header located at the top
 - of every page -}

headerCSS = do
    ".header" ?
        do margin0
           padding 0 (em 0.5) 0 (em 0.5)
           backgroundColor $ blue1
           color white
           h2 ?
             do fontSize $ em 1.6
                textAlign $ alignSide sideLeft
                width $ px 200
                display inlineBlock
                margin (px 10) 0 (px 5) 0
    "#nav-links" ?
        do
            "list-style" -: "none"
            width $ pct 70
            margin nil nil nil nil
            display inlineBlock
            a ?
              do fontWeight normal
            li <? do
                textAlign $ alignSide sideCenter
                display inlineBlock
                padding 0 (px 10) 0 (px 10)
                a <? do
                    color white
                    hover & do
                        color gray

{- aDefaultCSS
 - Generates default CSS. -}

aDefaultCSS = do
    a <> a # hover <> a # visited <> a # active ? do
        fontWeight bold
        textDecoration none
        color $ parse "#4C004C"

{- headers
 - Generates CSS for the header located at the top of
 - all pages. -}

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

{- disclaimerCSS
 - Generates CSS for the disclaimer located at the foot
 - of all pages. -}

disclaimerCSS = "#disclaimerDiv" ? do
    padding 0 (em 1) 0 (em 1)
    fontSize (pt 11)
