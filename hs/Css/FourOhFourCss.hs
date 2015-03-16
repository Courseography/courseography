{-# LANGUAGE OverloadedStrings #-}

module Css.FourOhFourCss where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Css.Constants

{- aboutStyles
 - Generates CSS for the about page. -}

fourOhFourStyles = do
	"#contentDiv" ? do
		margin nil (px 25) nil (px 25) 
	"#picDiv" ? do
		margin nil (px 25) nil (px 25)
		width $ px 1225
		height $ px 800
		"background-image" -: "url(http://1.bp.blogspot.com/-_jh9V05IGrg/Um7Vq-tvJ-I/AAAAAAAADD0/yrLmyK55hyM/s1600/dragon_smores_C.jpg)"
	"#links" ? do
		"list-style-type" -: "none"
