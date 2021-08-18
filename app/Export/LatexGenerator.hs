{-|
    Module      : Export.LatexGenerator
    Description : Contains functions for creating LaTeX text.
-}
module Export.LatexGenerator
    (generateTex) where

import Text.LaTeX
import Text.LaTeX.Packages.Fancyhdr
import Text.LaTeX.Packages.Geometry
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
    usepackage [] geometry
    applyGeometry [GLandscape True, GWidth (In 9)]
    let mySettings = defaultHdrSettings {leftHeader = "Graph and Timetables", rightHeader = "courseography.cdf.toronto.edu"}
    applyHdrSettings mySettings
    raw "\\pagenumbering{gobble}"

-- | Adds an includegraphics command for each image in imageNames. If an empty
-- list of imageNames was provided, the body will be empty.
body :: Monad m => [String] -> LaTeXT_ m
body [] = ""
body (imageName:imageNames) = do
    center $ includegraphics [IGWidth (CustomMeasure linewidth)] imageName
    body imageNames
