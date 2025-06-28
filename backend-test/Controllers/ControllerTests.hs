{-|
Description: Contoroller test suites module.

Module that contains the test suites for all the controllers.

-}

module Controllers.ControllerTests
(  controllerTests  ) where

import Test.Tasty

import Controllers.CourseControllerTests (courseControllerTestSuite)
import Controllers.GraphControllerTests (graphControllerTestSuite)

-- Single test encompassing all controller test suites
controllerTests :: TestTree
controllerTests = testGroup "Controller" [courseControllerTestSuite, graphControllerTestSuite]
