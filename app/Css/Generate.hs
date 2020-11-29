{-|
    Module      : Css.Generate
    Description : Defines the functions for the About page CSS.

Defines the functions which are later used in Compiler.hs.
-}
module Css.Generate
    (generateStyles) where

import Clay
import Prelude hiding ((**))
import Css.Constants

-- |Defines the CSS for the generate page.
generateStyles :: Css
generateStyles = "#generateDiv" ? do
    maxWidth (px 1000)
    padding 0 (em 1) 0 (em 1)
    margin nil auto nil auto
    textAlign justify
    h1 ? do
        color blue3