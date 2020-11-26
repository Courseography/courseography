module Css.Generate
    (generateStyles) where

import Clay
import Css.Constants

generateStyles :: Css
generateStyles = "generate-prerequisites" ? do
    border solid (px 1) black
    fontFamily ["Trebuchet MS", "Arial"] [sansSerif]
    alignCenter
    width (px 1016)
    height100
    marginTop (em 1)
    marginLeft auto
    marginRight auto
    ul ? do
            width $ pct 100
            margin0
            li ? do
                "list-style-type" -: "none"
                display inlineBlock
                width (pct 32)

