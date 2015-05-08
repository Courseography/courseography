{-# LANGUAGE OverloadedStrings #-}

module Css.FourOhFourCss where

import Clay

<<<<<<< HEAD
{- aboutStyles
 - Generates CSS for the about page. -}

=======
{- fourOhFourStyles
 - Generates CSS for the 404 page. -}
fourOhFourStyles :: Css
>>>>>>> 858b63c... Styling for privacy page
fourOhFourStyles = do
    "#contentDiv" ? do
        margin nil (px 25) nil (px 25) 
    "#picDiv" ? do
        margin nil (px 25) nil (px 25)
    "#links" ? do
        "list-style-type" -: "none"
