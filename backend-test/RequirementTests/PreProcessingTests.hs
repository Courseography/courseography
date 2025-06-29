{-|
Description: Test some pre-processing functions that clean up the text before running the parsers

-}

module RequirementTests.PreProcessingTests
( preProcTestSuite ) where

import Data.Text as T hiding (map)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.HTML.TagSoup (Tag (..))
import WebParsing.PostParser (pruneHtml)

createTest :: (Eq a, Show a, Eq b, Show b) => (a -> b) -> String -> [(a, b)] -> TestTree
createTest function label input = testGroup label $ Prelude.zipWith (\(x :: Int) (y, z) ->
                                testCase ("Test " ++ show x) $ assertEqual ("for (" ++ show z ++ ")")
                                z (function y)) [0..] input


pruneHtmlInputs :: [([Tag T.Text], [Tag T.Text])]
pruneHtmlInputs = [
    ( [TagOpen "h1" [("class", "c1 c2")], TagText "reqs", TagClose "h1"]
    , [TagOpen "h1" [], TagText"reqs", TagClose "h1"]
    ),
    ( [TagOpen "h2" [("style", "some: styles")], TagText "reqs", TagClose "h2"]
    , [TagOpen "h2" [("style", "some: styles")], TagText "reqs", TagClose "h2"]
    ),
    ( [TagOpen "h3" [("class", "c1 c2"), ("style", "some: styles")], TagText "reqs", TagClose "h3"]
    , [TagOpen "h3" [("style", "some: styles")], TagText "reqs", TagClose "h3"]
    ),
    ( [TagOpen "h4" [], TagOpen "a" [("href", "/CSC401H1")], TagText "CSC401H1", TagClose "a", TagClose "h4"]
    , [TagOpen "h4" [], TagText "CSC401H1", TagClose "h4"]
    )
    ]

pruneHtmlTests :: TestTree
pruneHtmlTests = createTest pruneHtml "filtering out html attributes" pruneHtmlInputs

-- functions for running tests in REPL
preProcTestSuite :: TestTree
preProcTestSuite = testGroup "Pre-processing tests" [pruneHtmlTests]
