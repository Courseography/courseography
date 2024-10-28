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
import Test.HUnit (Test (..))

-- Single test encompassing all requirement test suites
requirementTests :: Test
requirementTests = TestList [reqTestSuite, postTestSuite, preProcTestSuite, modifierTestSuite]