{-# LANGUAGE OverloadedStrings #-}

module Latex where

import Text.LaTeX
import Text.LaTeX.Packages.Graphicx

fitImageToPage =
    [IGWidth $ Pt 300.0,
     IGHeight $ Pt 300.0,
     KeepAspectRatio True,
     IGScale 1.0,
     IGAngle 0,
     IGTrim (Pt 0) (Pt 0) (Pt 0) (Pt 0),
     IGClip False,
     IGPage 1]

compileTex texName imageFilename = execLaTeXT (mainTex imageFilename) >>= renderFile texName 

mainTex imageFilename = do
    preamble
    body imageFilename

preamble = do
    documentclass [] article
    usepackage [] graphicx

body imageFilename = do
    addImage imageFilename

addImage imageFilename =
    includegraphics fitImageToPage imageFilename
