{-|
Description: SVG test suites module.

Module that contains the test suites for all the SVG tests.

-}

module SvgTests.SvgTests (svgTests) where

import SvgTests.IntersectionTests (intersectionTestSuite)
import Test.Tasty

-- Single test encompassing all svg test suites
svgTests :: TestTree
svgTests = testGroup "Svg" [intersectionTestSuite]
