{-# LANGUAGE OverloadedStrings #-}

module Css.CssGen where

import Clay
import Prelude hiding ((**))
import Data.Text.Lazy
import System.Directory
import Css.CommonCss
import Css.GraphCss
import Css.PostCss
import Css.TimetableCss
import Css.DrawCss
import Css.AboutCss
import Css.FourOhFourCss
import Css.SearchCss

styleFiles :: [(String, Css)]
styleFiles = [
    ("../public/style/common/common.css", common),
    ("../public/style/graph/graph_styles.css", graphStyles),
    ("../public/style/grid/timetable_styles.css", timetableStyles),
    ("../public/style/draw/draw_styles.css", drawStyles),
    ("../public/style/post/post_styles.css", postStyles),
    ("../public/style/common/about.css", aboutStyles),
    ("../public/style/common/four_oh_four.css", fourOhFourStyles),
    ("../public/style/search/search_styles.css", searchStyles)
    ]

renderStyleFile :: (String, Css) -> IO ()
renderStyleFile (path, css) = writeFile path $ unpack $ render css

generateCSS :: IO ()
generateCSS = do
    createDirectoryIfMissing True "../public/style/common"
    createDirectoryIfMissing True "../public/style/graph"
    createDirectoryIfMissing True "../public/style/grid"
    createDirectoryIfMissing True "../public/style/draw"
    createDirectoryIfMissing True "../public/style/post"
    createDirectoryIfMissing True "../public/style/search"
    Prelude.foldl1 (>>) $ Prelude.map renderStyleFile styleFiles
