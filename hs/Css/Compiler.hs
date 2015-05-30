{-# LANGUAGE OverloadedStrings #-}

module Css.Compiler where

import Prelude hiding (writeFile)
import Clay (renderWith)
import Data.Text.Lazy.IO (writeFile)
import Css.Common (common)
import Css.Graph (graphStyles)
import Css.Post (postStyles)
import Css.Timetable (timetableStyles)
import Css.Draw (drawStyles)
import Css.About (aboutStyles)
import Css.Privacy (privacyStyles)
import Css.FourOhFour (fourOhFourStyles)
import Css.Search (searchStyles)
import Config (genCssPath, cssStyle)
import System.Directory (createDirectoryIfMissing)

compileCSS :: IO ()
compileCSS = do
    createDirectoryIfMissing True genCssPath
    let cssText = renderWith cssStyle [] $ foldl1 (>>) [
                      common,
                      graphStyles,
                      timetableStyles,
                      drawStyles,
                      postStyles,
                      aboutStyles,
                      privacyStyles,
                      fourOhFourStyles,
                      searchStyles
                      ]
    writeFile (genCssPath ++ "app.css") cssText