module Scripts (
    graphScripts, timetableScripts, drawScripts, postScripts, searchScripts, generateScripts,
    globalScripts
    )
    where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Util.Blaze
import Config (enableCdn)

-- | Scripts that are loaded on every page.
globalScripts :: [T.Text]
globalScripts = jQueryScripts

jQueryScripts :: [T.Text]
jQueryScripts = if enableCdn
                then ["https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
                      "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js"]
                else ["/static/js/vendor/jquery.min.1.10.2.js",
                      "/static/js/vendor/jquery-ui.min.1.10.4.js"]

graphScripts :: H.Html
graphScripts = do
    mapM_ toScript $
        ["/static/js/vendor/bootstrap.min.3.1.1.js"]
    H.script ! A.src "/static/js/graph/app.js" $ ""

timetableScripts :: H.Html
timetableScripts = do
    mapM_ toScript $
        [if enableCdn
         then "https://maxcdn.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
         else "/static/js/vendor/bootstrap.min.3.1.1.js"
         ]
    H.script ! A.src "/static/js/grid/app.js" $ ""

drawScripts :: H.Html
drawScripts = do
    mapM_ toScript $
        ["/static/js/draw/variables.js",
         "/static/js/draw/path.js",
         "/static/js/draw/setup.js",
         "/static/js/vendor/jscolor.min.js"]
    H.script ! A.src "/static/js/draw/app.js" $ ""

postScripts :: H.Html
postScripts = do
    mapM_ toScript []
    H.script ! A.src "/static/js/post/app.js" $ ""

searchScripts :: H.Html
searchScripts = do
    H.script ! A.src "/static/js/search/app.js" $ ""

generateScripts :: H.Html
generateScripts = do
    H.ul ! A.id "generate-prerequisites" $ do
      H.li "CSS"
      H.li "is not"
      H.li "applied"
    -- H.script ! A.src "generate/generate.js" $ ""
