{-# LANGUAGE OverloadedStrings #-}

module Css.PrivacyCss where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

{- privacyStyles
 - Generates CSS for the Privacy page. -}
privacyStyles :: Css
privacyStyles = "#privacyDiv" ? do
    maxWidth (px 1000)
    padding 0 (em 1) 0 (em 1)
    margin nil auto nil auto
    textAlign justify
    h1 <> h2 <> h3 <? do
        color blue3
