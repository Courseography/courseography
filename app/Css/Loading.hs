{-|
    Module      : Css.Loading
    Description : Defines CSS for the loading animation.

Currently, the loading animation isn't utilized anywhere.
-}
module Css.Loading
    (loadingStyles) where

import Clay
import Prelude hiding ((**))

-- |Defines all CSS for the loading animation.
loadingStyles :: Css
loadingStyles = do
    spinnerCSS
    smallCompassPosCSS
    largeCompassPosCSS
    spinCSS

{- Properties of rotation animation. -}
spinnerCSS :: Css
spinnerCSS = ".spinner" ? do
    position relative
    "animation-name" -: "spin"
    "animation-duration" -: "2500ms"
    "animation-iteration-count" -: "infinite"
    "animation-timing-function" -: "linear"

{- Spin animation -}
spinCSS :: Css
spinCSS = keyframesFromTo "spin"
     (transform $ rotate (deg 0))
     (transform $ rotate (deg 360))

{- Small compass position. -}
smallCompassPosCSS :: Css
smallCompassPosCSS = "#compass-small" ? do
    left (px (-178))
    top (px (-9))

{- Large compass position. -}
largeCompassPosCSS :: Css
largeCompassPosCSS = "#compass" ? do
    left (px (-388))
    top (px (-33))
