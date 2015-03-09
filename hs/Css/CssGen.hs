{-# LANGUAGE OverloadedStrings #-}

module Css.CssGen where

import Clay
import Prelude hiding ((**))
import Data.Monoid
import Data.Text.Lazy
import System.Directory
import Css.Constants
import Css.CommonCss
import Css.GraphCss
import Css.PostCss
import Css.TimetableCss
import Css.DrawCss
import Css.AboutCss
import Css.FourOhFourCss

styleFiles :: [(String, Css)]
styleFiles = [
    ("../style/common/common.css", common),
    ("../style/graph/graph_styles.css", graphStyles),
    ("../style/grid/timetable_styles.css", timetableStyles),
    ("../style/draw/draw_styles.css", drawStyles),
    ("../style/post/post_styles.css", postStyles),
    ("../style/common/about.css", aboutStyles),
    ("../style/common/four_oh_four.css", fourOhFourStyles)
    ]

renderStyleFile :: (String, Css) -> IO ()
renderStyleFile (path, css) = writeFile path $ unpack $ render css

generateCSS :: IO ()
generateCSS = do
    createDirectoryIfMissing True "../style/common"
    createDirectoryIfMissing True "../style/graph"
    createDirectoryIfMissing True "../style/grid"
    createDirectoryIfMissing True "../style/draw"
    createDirectoryIfMissing True "../style/post"
    Prelude.foldl1 (>>) $ Prelude.map renderStyleFile styleFiles
