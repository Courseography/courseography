module SvgTests.BuilderTests where

import Svg.Builder
import Test.QuickCheck

prop1_sanitizeId str =
    let sanitized = sanitizeId str
        illegals  = map (\c -> not $ elem c ",()/<>% ") sanitized
        allTrue = and illegals
    in  allTrue
