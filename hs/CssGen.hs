{-# LANGUAGE OverloadedStrings #-}

module CssGen where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Data.Text.Lazy
import System.Directory
import Constants
import CommonCss
import GraphCss
import PostCss
import TimetableCss
import AboutCss

styleFiles :: [(String, Css)]
styleFiles = [
    ("../style/common/common.css", common),
    ("../style/graph/graph_styles.css", graphStyles),
    ("../style/grid/timetable_styles.css", timetableStyles),
    ("../style/common/about.css", aboutStyles),
    ("../style/post/post_styles.css", postStyles)
    ]

renderStyleFile :: (String, Css) -> IO ()
renderStyleFile (path, css) = writeFile path $ unpack $ render css

generateCSS :: IO ()
generateCSS = do
    createDirectoryIfMissing True "../style/common"
    createDirectoryIfMissing True "../style/graph"
    createDirectoryIfMissing True "../style/grid"
    createDirectoryIfMissing True "../style/post"
    Prelude.foldl1 (>>) $ Prelude.map renderStyleFile styleFiles
