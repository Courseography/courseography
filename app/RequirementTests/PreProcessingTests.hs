{-|
Description: Test some pre-processing functions for requirement parsing and database data.

-}

module RequirementTests.PreProcessingTests
( preProcTestSuite ) where

import Data.Text as T hiding (map)
import Test.HUnit (Test (..), assertEqual)
import Text.HTML.TagSoup (Tag (..))
import WebParsing.PostParser (pruneHtml)

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Eq a, Eq b, Show a, Show b) => (a -> b) -> String -> [(a, b)] -> Test
createTest function label input = TestLabel label $ TestList $ map (\(x, y) ->
                                TestCase $ assertEqual ("for (" ++ show y ++ ")")
                                y (function x)) input


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

pruneHtmlTests :: Test
pruneHtmlTests = createTest pruneHtml "filtering out html attributes" pruneHtmlInputs

-- functions for running tests in REPL
preProcTestSuite :: Test
preProcTestSuite = TestLabel "Pre-processing tests" $ TestList [pruneHtmlTests]
