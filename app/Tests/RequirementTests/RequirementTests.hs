{-|
Description: Requirement test suites module.

Module that contains the test suites for all the requirement tests.

-}

module Tests.RequirementTests.RequirementTests
(  requirementTests  ) where

import Test.HUnit (Test (..))
import Tests.RequirementTests.ModifierTests (modifierTestSuite)
import Tests.RequirementTests.PostParserTests (postTestSuite)
import Tests.RequirementTests.PreProcessingTests (preProcTestSuite)
import Tests.RequirementTests.ReqParserTests (reqTestSuite)

-- Single test encompassing all requirement test suites
requirementTests :: Test
requirementTests = TestList [reqTestSuite, postTestSuite, preProcTestSuite, modifierTestSuite]
