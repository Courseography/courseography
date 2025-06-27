{-|
Description: Requirement test suites module.

Module that contains the test suites for all the requirement tests.

-}

module RequirementTests.RequirementTests
(  requirementTests  ) where

import RequirementTests.ModifierTests (modifierTestSuite)
import RequirementTests.PostParserTests (postTestSuite)
import RequirementTests.PreProcessingTests (preProcTestSuite)
import RequirementTests.ReqParserTests (reqTestSuite)
import Test.Tasty

-- Single test encompassing all requirement test suites
requirementTests :: TestTree
requirementTests = testGroup "Requirements" [modifierTestSuite, preProcTestSuite, postTestSuite, reqTestSuite]
