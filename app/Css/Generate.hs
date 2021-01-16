{-|
    Module      : Css.Generate
    Description : Defines the functions for the Generate page CSS.

Defines the functions which are later used in Compiler.hs.
-}
module Css.Generate
    (generateStyles) where

import Clay
import Prelude hiding ((**))
import Css.Constants

-- |Defines the CSS for the generate page.
generateStyles :: Css
generateStyles = do
    "#generateDiv" ? do
        backgroundColor purple10
        width (px 500)
        height100
        position absolute
        ul ? do
            paddingLeft (px 5)
            "list-style-type" -: "none"
            li ? do
                fontSize (px 10)
                color "#d9e4ee"
                paddingLeft (px 23)
                paddingRight (px 10)
        input ? do
            color purple10
            backgroundColor "#d9e4ee"
            textAlign $ alignSide sideCenter
            paddingTop (px 5)
            paddingBottom (px 5)
            paddingLeft (px 70)
            paddingRight (px 70)
            border solid (px 5) purple8
    "#header" ? do
        backgroundColor purple10
        width (px 160)
    "#header-title" ? do
        color "#d9e4ee"
        paddingTop (px 5)
        textAlign $ alignSide sideCenter
        fontSize (px 30)
        fontWeight bold
    "main-filter" ? do
        textAlign $ alignSide sideCenter
        marginTop (px 30)
        input ? do
            border solid (px 5) purple10
    "#filter-title" ? do
        color "#d9e4ee"
        paddingLeft (px 23)
        paddingTop (px 10)
        textAlign $ alignSide sideCenter
        fontSize (px 20)
        fontWeight bold
    "#header" ? do
        color purple10
        fontWeight bolder
    "#submit" ? do
        textAlign $ alignSide sideCenter
    "#submit" ? do
        fontWeight bolder
        color purple10
        button ? do
            color "#d9e4ee"
            paddingTop (px 5)
            textAlign $ alignSide sideCenter
            fontSize (px 30)
            fontWeight bold
