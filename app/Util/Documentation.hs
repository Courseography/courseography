{-|
    Module      : Util.Documentation
    Description : Defines function to generate local Haddock documentation.

This module contains a shell command to run Haddock to generate documentation
for the Haskell source files of Courseography.
-}
module Util.Documentation where

import System.Directory (createDirectoryIfMissing)
import System.Process (callCommand)

-- | Path to documentation directory
docPath :: String
docPath = "doc"

-- | Generate documentation for Courseography.
generateDocs :: IO ()
generateDocs = do
  putStrLn "Generating documentation..."
  createDirectoryIfMissing True docPath
  callCommand $ unwords [
    "stack exec haddock --",
    "-o",
    docPath,
    "-h",
    "--optghc=-iapp",
    "--optghc=-XOverloadedStrings",
    "--optghc=-XPartialTypeSignatures",
    "--optghc=-XScopedTypeVariables",
    "--ignore-all-exports",
    "app/Main.hs"
    ]
