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
                else ["/static/js/common/jquery.min.1.10.2.js",
                      "/static/js/common/jquery-ui.min.1.10.4.js"]

reactScripts :: [String]
reactScripts = if enableCdn
               then ["https://cdnjs.cloudflare.com/ajax/libs/react/0.13.1/react.js",
                     "https://cdnjs.cloudflare.com/ajax/libs/react/0.13.1/JSXTransformer.js"]
               else ["/static/js/common/react.0.13.1.js",
                     "/static/js/common/JSXTransformer.0.13.1.js"]

analyticsScripts :: [String]
analyticsScripts = [
    "/static/js/common/google_analytics.js"
    ]

graphScripts :: H.Html
graphScripts = do
    sequence_ (map toScript $
        ["/static/js/graph/tooltip.js",
         "/static/js/common/course_videos.js",
         "/static/js/common/modal.js",
         "/static/js/graph/objects/edge.js",
         "/static/js/graph/objects/node.js",
         "/static/js/common/objects/course.js",
         "/static/js/common/cookie_handler.js",
         "/static/js/common/course_description.js",
         "/static/js/graph/sidebar/focuses.js",
         "/static/js/graph/sidebar/fce_count.js",
         "/static/js/common/objects/section.js",
         "/static/js/common/utilities/util.js",
         "/static/js/graph/utilities/structs.js",
         "/static/js/graph/utilities/util.js",
         "/static/js/graph/create_data.js",
         "/static/js/graph/parse_graph.js",
         "/static/js/graph/mouse_events.js",
         "/static/js/common/image_conversion.js",
         "/static/js/common/graph_image.js",
         "/static/js/graph/setup.js",
         "/static/js/post/update_post.js",
         "/static/js/graph/sidebar/sidebar_divs.js",
         "/static/js/graph/sidebar/sidebar_events.js",
         "/static/js/graph/sidebar/focus_descriptions.js",
         "/static/js/common/export/export.js"])
    H.script ! A.type_ "text/jsx" ! A.src "/static/js/common/react_modal.js.jsx" $ ""

timetableScripts :: H.Html
timetableScripts = do
    sequence_ (map toScript $
        ["/static/js/grid/mouse_events.js",
         (if enableCdn
          then "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
          else "/static/js/common/bootstrap.min.3.1.1.js"),
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
    H.script ! H.dataAttribute "main" "/static/js/grid" ! A.src "/static/js/require.js" $ ""

drawScripts :: H.Html
drawScripts = do
    sequence_ (map toScript $
        ["/static/js/draw/variables.js",
         "/static/js/draw/path.js",
         "/static/js/draw/draw.js",
         "/static/js/draw/setup.js"])

postScripts :: H.Html
postScripts = sequence_ (map toScript [
                                          "/static/js/post/change_div.js",
                                          "/static/js/common/cookie_handler.js",
                                          "/static/js/post/update_post.js",
                                          "/static/js/graph/create_data.js",
                                          "/static/js/graph/objects/node.js",
                                          "/static/js/post/fill_textboxes.js",
                                          "/static/js/graph/create_data.js",
                                          "/static/js/post/update_categories.js"])

searchScripts :: H.Html
searchScripts =
    H.script ! A.type_ "text/jsx" ! A.src "/static/js/search/timetable.js.jsx" $ ""
