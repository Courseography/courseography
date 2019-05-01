{-|
    Module      : Css.About
    Description : Defines the functions for the About page CSS.

Defines the functions which are later used in Compiler.hs.
-}
module Css.About
    (aboutStyles) where

import Clay
import Prelude hiding ((**))
import Css.Constants

-- |Defines the CSS for the about page.
aboutStyles :: Css
aboutStyles = "#aboutDiv" ? do
    maxWidth (px 1000)
    padding 0 (em 1) 0 (em 1)
    margin nil auto nil auto
    textAlign justify
    h1 <> h2 <> h3 <? do
        color blue3
