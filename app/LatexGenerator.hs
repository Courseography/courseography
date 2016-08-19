{-# LANGUAGE OverloadedStrings #-}

module LatexGenerator
    (generateTex) where

import Text.LaTeX
import Text.LaTeX.Packages.Graphicx

-- | Create a TEX file named texName that includes all of the images in
-- imageNames
generateTex :: [String] -> String -> IO ()
generateTex imageNames texName = execLaTeXT (buildTex imageNames) >>= renderFile texName

-- | Combine the preamble and the document text into a single block of latex
-- code. The document text contains code to insert all of the images in
-- imageNames.
buildTex :: Monad m => [String] -> LaTeXT_ m
buildTex imageNames = do
    preamble
    document (body imageNames)

-- | Defines documentclass and packages used.
preamble :: Monad m => LaTeXT_ m
preamble = do
    documentclass [] article
    usepackage [] graphicx

-- | Adds an includegraphics command for each image in imageNames. If an empty
-- list of imageNames was provided, the body will be empty.
body :: Monad m => [String] -> LaTeXT_ m
body [] = ""
body (imageName:imageNames) = do
    center $ includegraphics [IGWidth (CustomMeasure linewidth)] imageName
    body imageNames
