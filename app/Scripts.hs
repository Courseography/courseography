module Scripts (
    graphScripts, timetableScripts, drawScripts, postScripts, searchScripts, generateScripts, aboutScripts
    )
    where

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util.Blaze

graphScripts :: H.Html
graphScripts = H.script ! A.src "/static/js/graph/app.js" $ ""

timetableScripts :: H.Html
timetableScripts = H.script ! A.src "/static/js/grid/app.js" $ ""

drawScripts :: H.Html
drawScripts = do
    mapM_ toScript
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
    H.script ! A.src "/static/js/generate/app.js" $ ""

aboutScripts :: H.Html
aboutScripts = do 
    H.script ! A.src "/static/js/about/aboutPage.js" $ ""
