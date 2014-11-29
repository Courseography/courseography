{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | A renderer that produces a native Haskell 'String', mostly meant for
-- debugging purposes.
--
module Text.Blaze.Svg.Renderer.String
    ( renderSvg
    ) where

import Text.Blaze.Renderer.String (renderMarkup)

renderSvg = renderMarkup