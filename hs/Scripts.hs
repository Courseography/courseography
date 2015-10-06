{-# LANGUAGE OverloadedStrings #-}

module Scripts (
    graphScripts, timetableScripts, drawScripts, postScripts, searchScripts, notFoundScripts,
    jQueryScripts
    )
    where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util.Blaze
import Config (enableFb)

facebookScripts :: [String]
facebookScripts = [
    "/static/js/common/facebook/facebook_login.js",
    "/static/js/common/facebook/facebook_image.js",
    "/static/js/common/facebook/facebook_modal.js"
    ]

jQueryScripts :: [String]
jQueryScripts =
    ["https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
     "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js"]

graphScripts :: H.Html
graphScripts = sequence_ (map toScript $
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
     "/static/js/common/export/export.js"] ++
    if enableFb then facebookScripts else [])

timetableScripts :: H.Html
timetableScripts = do
    sequence_ (map toScript $
        ["/static/js/grid/mouse_events.js",
         "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js",
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
         "/static/js/common/export/export.js"] ++
        if enableFb then facebookScripts else [])
    H.script ! H.dataAttribute "main" "/static/js/grid" ! A.src "/static/js/require.js" $ ""

drawScripts :: H.Html
drawScripts = do
    sequence_ (map toScript $
        ["/static/js/draw/variables.js",
         "/static/js/draw/path.js",
         "/static/js/draw/draw.js",
         "/static/js/draw/setup.js"])

postScripts :: H.Html
postScripts = do 
    sequence_ (map toScript [
                                          "/static/js/post/change_div.js",
                                          "/static/js/common/cookie_handler.js",
                                          "/static/js/post/update_post.js",
                                          "/static/js/graph/create_data.js",
                                          "/static/js/graph/objects/node.js",
                                          "/static/js/post/fill_textboxes.js",
                                          "/static/js/graph/create_data.js",
                                          "/static/js/post/update_categories.js",
                                          "static/js/common/course_description.js",
                                          "/static/js/common/objects/course.js",
                                          "/static/js/common/objects/section.js",
                                          "/static/js/common/utilities/util.js",
                                          "https://cdnjs.cloudflare.com/ajax/libs/react/0.13.1/react.js",
         "https://cdnjs.cloudflare.com/ajax/libs/react/0.13.1/JSXTransformer.js"])
    H.script ! A.type_ "text/jsx" ! A.src "/static/js/post/post.js" $ ""        
searchScripts :: H.Html
searchScripts = do
    sequence_ (map toScript $
        ["https://cdnjs.cloudflare.com/ajax/libs/react/0.13.1/react.js",
         "https://cdnjs.cloudflare.com/ajax/libs/react/0.13.1/JSXTransformer.js"])
    H.script ! A.type_ "text/jsx" ! A.src "/static/js/search/timetable.js" $ ""

notFoundScripts :: H.Html
notFoundScripts = sequence_ $ map toScript jQueryScripts
