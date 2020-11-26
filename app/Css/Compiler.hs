{-|
    Module      : Css.Compiler
    Description : Generates the CSS file for Courseography.

Uses the all of the other CSS files contained in app/Css to and creates
the file containing Courseography's CSS. Effectively this is the main
file of app/Css.
-}
module Css.Compiler
    (compileCSS) where

import Prelude hiding (writeFile)
import Clay (renderWith)
import Data.Text.Lazy.IO (writeFile)
import Css.Common (common)
import Css.Generate (generateStyles)
import Css.Graph (graphStyles)
import Css.Post (postStyles)
import Css.Timetable (timetableStyles)
import Css.Draw (drawStyles)
import Css.About (aboutStyles)
import Css.Privacy (privacyStyles)
import Css.FourOhFour (fourOhFourStyles)
import Css.Search (searchStyles)
import Css.Loading (loadingStyles)
import Config (genCssPath, cssStyle)
import System.Directory (createDirectoryIfMissing)

-- |Combines the other app/Css files and generates the Courseography CSS file.
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
                      searchStyles,
                      generateStyles,
                      loadingStyles
                      ]
    writeFile (genCssPath ++ "app.css") cssText
