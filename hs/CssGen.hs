{-# LANGUAGE OverloadedStrings #-}

module CssGen where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Data.Text.Lazy
import System.Directory
import Consts
import CommonCssGen as COMMON
import GraphCssGen as GRAPH
import TimetableCssGen as TIMETABLE
import AboutCssGen as ABOUT

styleFiles :: [(String, Css)]
styleFiles = [
    ("../style/common/common.css", COMMON.common),
    ("../style/graph/graph_styles.css", GRAPH.graphStyles),
    ("../style/grid/timetable_styles.css", TIMETABLE.timetableStyles),
    ("../style/common/about.css", ABOUT.aboutStyles)
    ]

renderStyleFile :: (String, Css) -> IO ()
renderStyleFile (path, css) = writeFile path $ unpack $ render css

generateCSS :: IO ()
generateCSS = do
    createDirectoryIfMissing True "../style/common"
    createDirectoryIfMissing True "../style/graph"
    createDirectoryIfMissing True "../style/grid"
    Prelude.foldl1 (>>) $ Prelude.map renderStyleFile styleFiles

-- Currently not used
fceCountCSS = "#FCECountDiv" ? do
    backgroundColor fceCountColor
    float floatRight
    padding0
    lineHeight (px 40)
    "vertical-align" -: "middle"
    fontSize (em 1.35)
    alignCenter

