-- | Tests for graph image export.
module Export.GetImagesTests (test_graphImageExport) where

import Config (runDb)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.Persist.Sqlite (insert_)
import Database.Tables (Graph (Graph))
import Export.GetImages (writeActiveGraphImage)
import System.IO (SeekMode (AbsoluteSeek), hSeek)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestHelpers (clearDatabase, withDatabase)

-- | Missing graph names use the default; explicit graph names are preserved.
testActiveGraphImage :: TestTree
testActiveGraphImage = testCase "Graph image uses the default or explicitly selected graph" $ do
    runDb $ do
        clearDatabase
        insert_ (Graph "Computer Science" 120 60 False)
        insert_ (Graph "Statistics" 240 100 False)

    let renderGraph graphInfo =
            withSystemTempFile "graph-export.svg" $ \_ handle -> do
                writeActiveGraphImage graphInfo handle
                hSeek handle AbsoluteSeek 0
                TIO.hGetContents handle

    defaultSvg <- renderGraph "{}"
    explicitDefaultSvg <- renderGraph "{\"active-graph\":\"Computer Science\"}"
    statisticsSvg <- renderGraph "{\"active-graph\":\"Statistics\"}"

    assertBool "The default graph should produce the Computer Science SVG" $
        T.isInfixOf "0 0 120.0 60.0" defaultSvg
    assertEqual "The missing and explicit default should produce the same SVG" explicitDefaultSvg defaultSvg
    assertBool "An explicit selection should produce the selected graph's SVG" $
        T.isInfixOf "0 0 240.0 100.0" statisticsSvg

test_graphImageExport :: TestTree
test_graphImageExport = withDatabase "Graph image export tests" [testActiveGraphImage]
