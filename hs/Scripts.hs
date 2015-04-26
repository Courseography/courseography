{-# LANGUAGE OverloadedStrings #-}

module Scripts where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import MakeElements

plannerScripts :: H.Html
plannerScripts = concatHtml [makeScript "static/js/pages/graph.bundle.js"]

timetableScripts :: H.Html
timetableScripts = do jQuery
                      concatHtml (map makeScript ["static/js/grid/timetable_util.js",
                                                "/static/js/grid/setup.js",
                                                "/static/js/grid/mouse_events.js",
                                                "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js",
                                                "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js",
                                                "/static/js/common/cookie_handler.js",
                                                "/static/js/grid/generate_grid.js",
                                                "/static/js/common/objects/course.js",
                                                "/static/js/common/objects/section.js",
                                                "/static/js/common/utilities/util.js",
                                                "/static/js/common/facebook/facebook_login.js",
                                                "/static/js/common/facebook/facebook_image.js",
                                                "/static/js/common/facebook/facebook_modal.js",
                                                "/static/js/common/grid_image.js",
                                                "/static/js/common/image_conversion.js",
                                                "/static/js/draw/draw.js",
                                                "static/js/common/modal.js",
                                                "static/js/common/course_description.js"])

drawScripts :: H.Html
drawScripts = do jQuery
                 concatHtml (map makeScript ["https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js",
                                             "static/js/draw/variables.js",
                                             "static/js/draw/path.js",
                                             "static/js/draw/draw.js",
                                             "static/js/draw/setup.js"])

postScripts :: H.Html
postScripts = concatHtml (map makeScript ["https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
                                          "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js",
                                          "static/js/post/change_div.js",
                                          "static/js/common/cookie_handler.js",
                                          "static/js/post/update_post.js",
                                          "static/js/graph/create_data.js",
                                          "static/js/graph/objects/node.js",
                                          "static/js/post/fill_textboxes.js",
                                          "static/js/graph/create_data.js",
                                          "static/js/post/update_categories.js"])

searchScripts :: H.Html
searchScripts = do
    concatHtml (map makeScript ["https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
                                          "https://cdnjs.cloudflare.com/ajax/libs/react/0.13.1/react.js",
                                          "https://cdnjs.cloudflare.com/ajax/libs/react/0.13.1/JSXTransformer.js"])
    H.script ! A.type_ "text/jsx" ! A.src "static/js/search/timetable.js" $ ""

fourOhFourScripts :: H.Html
fourOhFourScripts = jQuery
