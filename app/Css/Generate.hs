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
    "#generateRoot" ? do
        height100
    "#generateRoot #react-graph" ? do
        position static
    "#generateDiv" ? do
        color "#d9e4ee"
        backgroundColor purple10
        width (px 500)
        height100
        ul ? do
            paddingLeft (px 5)
            "list-style-type" -: "none"
            li ? do
                fontSize (pt 12)
                color "#d9e4ee"
                paddingLeft (px 23)
                paddingRight (px 10)
        input ? do
            color purple10
            backgroundColor "#d9e4ee"
            margin0
            "type=text" & do
                width (pct 100)
        label ? do
            fontWeight normal
    "#header-title" ? do
        fontSize (em 1.2)
    "main-filter" ? do
        textAlign $ alignSide sideCenter
        marginTop (px 30)
        input ? do
            border solid (px 5) purple10
    "#filter-title" ? do
        color "#d9e4ee"
        fontSize (px 20)
    "#submit" ? do
        color purple10
