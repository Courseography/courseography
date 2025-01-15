{-|
Description: Contoroller test suites module.

Module that contains the test suites for all the controllers.

-}

module Controllers.ControllerTests
(  controllerTests  ) where

import Test.HUnit (Test (..))
import Controllers.CourseControllerTests (courseControllerTestSuite)

-- Single test encompassing all controller test suites
controllerTests :: Test
controllerTests = TestList [courseControllerTestSuite]