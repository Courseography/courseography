{-# LANGUAGE OverloadedStrings #-}

module Scripts (
    graphScripts, timetableScripts, drawScripts, postScripts, searchScripts,
    globalScripts
    )
    where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util.Blaze
import Config (enableFb, enableCdn)

-- | Scripts that are loaded on every page.
globalScripts :: [String]
globalScripts =
    concat [jQueryScripts, reactScripts, analyticsScripts] ++
    if enableFb then facebookScripts else []

facebookScripts :: [String]
facebookScripts = [
    "/static/js/common/facebook/facebook_login.js",
    "/static/js/common/facebook/facebook_image.js",
    "/static/js/common/facebook/facebook_modal.js"
    ]

jQueryScripts :: [String]
jQueryScripts = if enableCdn
                then ["https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
                      "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js"]
                else ["/static/js/vendor/jquery.min.1.10.2.js",
                      "/static/js/vendor/jquery-ui.min.1.10.4.js"]

reactScripts :: [String]
reactScripts = if enableCdn
               then ["https://cdnjs.cloudflare.com/ajax/libs/react/0.14.3/react.min.js",
                     "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.3/react-dom.min.js",
                     "https://cdnjs.cloudflare.com/ajax/libs/babel-core/5.8.34/browser.min.js"]
               else ["/static/js/vendor/react.0.14.3.js",
                     "/static/js/vendor/react-dom.0.14.3.js",
                     "/static/js/vendor/browser.5.8.34.js"]

analyticsScripts :: [String]
analyticsScripts = [
    "/static/js/common/google_analytics.js"
    ]

graphScripts :: H.Html
graphScripts = do
    sequence_ (map toScript $
        ["/static/js/common/course_videos.js",
         "/static/js/common/modal.js",
         "/static/js/common/objects/course.js",
         "/static/js/common/cookie_handler.js",
         "/static/js/common/course_description.js",
         "/static/js/common/objects/section.js",
         "/static/js/common/utilities/util.js",
         "/static/js/common/image_conversion.js",
         "/static/js/common/graph_image.js",
         "/static/js/vendor/bootstrap.min.3.1.1.js",
         "/static/js/common/export/export.js",
         "/static/js/common/modal.js"])
    H.script ! A.src "/static/js/requirejs-config.js" $ ""
    H.script ! H.dataAttribute "main" "/static/js/graph" ! A.src "/static/js/vendor/require.js" $ ""

timetableScripts :: H.Html
timetableScripts = do
    sequence_ (map toScript $
        ["/static/js/grid/mouse_events.js",
         (if enableCdn
          then "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
          else "/static/js/vendor/bootstrap.min.3.1.1.js"),
         "/static/js/common/cookie_handler.js",
         "/static/js/common/objects/course.js",
         "/static/js/common/objects/section.js",
         "/static/js/common/utilities/util.js",
         "/static/js/common/grid_image.js",
         "/static/js/common/image_conversion.js",
         "/static/js/draw/draw.js",
         "/static/js/common/course_videos.js",
         "/static/js/common/modal.js",
         "/static/js/common/course_description.js",
         "/static/js/common/export/export.js"])
    H.script ! H.dataAttribute "main" "/static/js/grid" ! A.src "/static/js/vendor/require.js" $ ""

drawScripts :: H.Html
drawScripts = do
    sequence_ (map toScript $
        ["/static/js/draw/variables.js",
         "/static/js/draw/path.js",
         "/static/js/draw/draw.js",
         "/static/js/draw/setup.js",
         "/static/js/vendor/jscolor.min.js"])

postScripts :: H.Html
postScripts = do
    sequence_ (map toScript [
                                          "/static/js/common/cookie_handler.js",
                                          "/static/js/graph/create_data.js",
                                          "/static/js/graph/create_data.js",
                                          "/static/js/common/course_description.js",
                                          "/static/js/common/objects/course.js",
                                          "/static/js/common/objects/section.js",
                                          "/static/js/common/utilities/util.js",
                                          "/static/js/common/modal.js",
                                          "/static/js/vendor/bootstrap.min.3.1.1.js",
                                          "/static/js/common/course_videos.js"])
    H.script ! A.src "/static/js/requirejs-config.js" $ ""
    H.script ! H.dataAttribute "main" "/static/js/post" ! A.src "/static/js/vendor/require.js" $ ""

searchScripts :: H.Html
searchScripts =
    H.script ! A.type_ "text/babel" ! A.src "/static/js/search/timetable.js.jsx" $ ""
