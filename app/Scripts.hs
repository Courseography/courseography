module Scripts (
    graphScripts,
    timetableScripts,
    generateScripts,
    aboutScripts,
)
where

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

graphScripts :: H.Html
graphScripts = H.script ! A.src "/static/js/graph/app.js" $ ""

timetableScripts :: H.Html
timetableScripts = H.script ! A.src "/static/js/grid/app.js" $ ""

generateScripts :: H.Html
generateScripts = do
    H.script ! A.src "/static/js/generate/app.js" $ ""

aboutScripts :: H.Html
aboutScripts = do
    H.script ! A.src "/static/js/about/app.js" $ ""
