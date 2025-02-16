{-|
Description: SVG test suites module.

Module that contains the test suites for all the SVG tests.

-}

module SvgTests.SvgTests (svgTests) where

import Test.HUnit (Test (..))
import SvgTests.IntersectionTests (intersectionTestSuite)

-- Single test encompassing all svg test suites
svgTests :: Test
svgTests = TestList [intersectionTestSuite]
