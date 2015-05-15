{-# LANGUAGE OverloadedStrings #-}

module Css.Compiler where

import Clay
import Prelude hiding ((**))
import Data.Text.Lazy
import System.Directory
import Css.Common
import Css.Graph
import Css.Post
import Css.Timetable
import Css.Draw
import Css.About
import Css.Privacy
import Css.FourOhFour
import Css.Search
import Config (genCssPath)

styleFiles :: [(String, Css)]
styleFiles = [
    (genCssPath ++ "common/common.css", common),
    (genCssPath ++ "graph/graph_styles.css", graphStyles),
    (genCssPath ++ "grid/timetable_styles.css", timetableStyles),
    (genCssPath ++ "draw/draw_styles.css", drawStyles),
    (genCssPath ++ "post/post_styles.css", postStyles),
    (genCssPath ++ "common/about.css", aboutStyles),
    (genCssPath ++ "common/privacy.css", privacyStyles),
    (genCssPath ++ "common/four_oh_four.css", fourOhFourStyles),
    (genCssPath ++ "search/search_styles.css", searchStyles)
    ]

renderStyleFile :: (String, Css) -> IO ()
renderStyleFile (path, css) = writeFile path $ unpack $ render css

compileCSS :: IO ()
compileCSS = do
    createDirectoryIfMissing True $ genCssPath ++ "common"
    createDirectoryIfMissing True $ genCssPath ++ "graph"
    createDirectoryIfMissing True $ genCssPath ++ "grid"
    createDirectoryIfMissing True $ genCssPath ++ "draw"
    createDirectoryIfMissing True $ genCssPath ++ "post"
    createDirectoryIfMissing True $ genCssPath ++ "search"
    Prelude.foldl1 (>>) $ Prelude.map renderStyleFile styleFiles
