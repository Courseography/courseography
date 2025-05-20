{-|
Description: Contoroller test suites module.

Module that contains the test suites for all the controllers.

-}

module Controllers.ControllerTests
(  controllerTests  ) where

import Controllers.CourseControllerTests (courseControllerTestSuite)
import Controllers.GraphControllerTests (graphControllerTestSuite)
import Test.HUnit (Test (..))

-- Single test encompassing all controller test suites
controllerTests :: Test
controllerTests = TestList [courseControllerTestSuite, graphControllerTestSuite]
