{-|
    Module      : Css.FourOhFour
    Description : Defines CSS for the 404 page.
-}
module Css.FourOhFour
    (fourOhFourStyles) where

import Clay

-- |CSS for the 404 page.
fourOhFourStyles :: Css
fourOhFourStyles = do
    "#contentDiv" ? do
        margin nil (px 25) nil (px 25)
    "#picDiv" ? do
        margin nil (px 25) nil (px 25)
    "#links" ? do
        "list-style-type" -: "none"
