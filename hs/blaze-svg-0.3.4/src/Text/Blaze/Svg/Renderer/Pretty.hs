{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | A renderer that produces pretty SVG, mostly meant for debugging purposes.
--
module Text.Blaze.Svg.Renderer.Pretty
    ( renderSvg
    ) where

import Text.Blaze.Renderer.Pretty (renderMarkup)

renderSvg = renderMarkup