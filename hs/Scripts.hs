{-# LANGUAGE OverloadedStrings #-}

module Scripts where

import Data.List
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import MakeElements
import MasterTemplate

plannerScripts :: H.Html
plannerScripts = concatHtml (map makeScript["https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
                                           "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js",
                                           "static/js/graph/modal.js",
                                           "static/js/graph/objects/edge.js",
                                           "static/js/graph/objects/node.js",
                                           "static/js/common/objects/course.js",
                                           "static/js/common/cookieHandler.js",
                                           "static/js/graph/tabs/setup_tabs.js",
                                           "static/js/graph/utilities/course_description.js",
                                           "static/js/graph/tabs/feedback_form.js",
                                           "static/js/graph/tabs/focuses.js",
                                           "static/js/graph/tabs/post.js",
                                           "static/js/graph/tabs/timetable.js",
                                           "static/js/graph/tabs/fce_count.js",
                                           "static/js/common/objects/section.js",
                                           "static/js/common/utilities/util.js",
                                           "static/js/graph/utilities/structs.js",
                                           "static/js/graph/utilities/util.js",
                                           "static/js/graph/create_data.js",
                                           "static/js/graph/parse_graph.js",
                                           "static/js/graph/mouse_events.js",
                                           "static/js/graph/setup.js", 
                                           "static/js/post/update_post.js",
                                           "static/js/post/update_counts.js"])

timetableScripts :: H.Html
timetableScripts = do jQuery
                      concatHtml (map makeScript ["static/js/grid/timetable_util.js",
                                                "static/js/grid/setup.js",
                                                "static/js/grid/mouse_events.js",
                                                "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js",
                                                "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js",
                                                "static/js/common/cookieHandler.js",
                                                "static/js/grid/generate_grid.js",
                                                "static/js/common/objects/course.js",
                                                "static/js/common/objects/section.js",
                                                "static/js/common/utilities/util.js",
                                                "static/js/draw/draw.js"])

--drawScripts :: H.Html
--drawScripts = do jQuery 
--                  concatHtml (map makeScript ["static/js/experiment.js"])

postScripts :: H.Html
postScripts = concatHtml (map makeScript ["https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
                                          "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.4/jquery-ui.min.js",
                                          "static/js/post/change_div.js",
                                          "static/js/common/cookieHandler.js",
                                          "static/js/post/update_post.js",
                                          "static/js/graph/create_data.js",
                                          "static/js/post/update_counts.js",
                                          "static/js/graph/objects/node.js",
                                          "static/js/post/fill_textboxes.js"])