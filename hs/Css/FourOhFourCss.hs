{-# LANGUAGE OverloadedStrings #-}

module Css.FourOhFourCss where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

{- aboutStyles
 - Generates CSS for the about page. -}

fourOhFourStyles = "#picDiv" ? do
	width $ px 1100
	height $ px 750
	"background-image" -: "url(http://1.bp.blogspot.com/-_jh9V05IGrg/Um7Vq-tvJ-I/AAAAAAAADD0/yrLmyK55hyM/s1600/dragon_smores_C.jpg)"