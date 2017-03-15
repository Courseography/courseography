{-|
    Module      : Util.Documentation
    Description : Defines function to generate local Haddock documentation.

This module contains a shell command to run Haddock to generate documentation
for the Haskell source files of Courseography.
-}
module Util.Documentation where

import Data.List (intercalate)
import System.Process (callCommand)
import System.Directory (createDirectoryIfMissing)

-- | Path to documentation directory
docPath :: String
docPath = "doc"

-- | Generate documentation for Courseography.
generateDocs :: IO ()
generateDocs = do
  print "Generating documentation..."
  createDirectoryIfMissing True docPath
  callCommand $ intercalate " " [
    "stack exec haddock --",
    "-o",
    docPath,
    "-h",
    "--optghc=\"-iapp\"",
    "--ignore-all-exports",
    "app/Main.hs"
    ]
