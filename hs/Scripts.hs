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
plannerScripts = concatHtml (map makeScript["http://code.jquery.com/jquery-1.10.2.js",
                                            "http://code.jquery.com/ui/1.10.4/jquery-ui.js",
                                            "http://localhost:8000/static/js/graph/modal.js",
                                            "http://localhost:8000/static/js/graph/objects/edge.js",
                                            "http://localhost:8000/static/js/graph/objects/node.js",
                                            "http://localhost:8000/static/js/common/objects/course.js",
                                            "http://localhost:8000/static/js/common/facebook_login.js",
                                            "http://localhost:8000/static/js/common/cookieHandler.js",
                                            "http://localhost:8000/static/js/graph/tabs/setup_tabs.js",
                                            "http://localhost:8000/static/js/graph/utilities/course_description.js",
                                            "http://localhost:8000/static/js/graph/tabs/feedback_form.js",
                                            "http://localhost:8000/static/js/graph/tabs/focuses.js",
                                            "http://localhost:8000/static/js/graph/tabs/post.js",
                                            "http://localhost:8000/static/js/graph/tabs/timetable.js",
                                            "http://localhost:8000/static/js/graph/tabs/fce_count.js",
                                            "http://localhost:8000/static/js/common/objects/section.js",
                                            "http://localhost:8000/static/js/common/utilities/util.js",
                                            "http://localhost:8000/static/js/graph/utilities/structs.js",
                                            "http://localhost:8000/static/js/graph/utilities/util.js",
                                            "http://localhost:8000/static/js/graph/create_data.js",
                                            "http://localhost:8000/static/js/graph/parse_graph.js",
                                            "http://localhost:8000/static/js/graph/mouse_events.js",
                                            "http://localhost:8000/static/js/graph/setup.js"])

timetableScripts :: H.Html
timetableScripts = do jQuery
                      concatHtml (map makeScript ["http://localhost:8000/static/js/grid/timetable_util.js",
                                                  "http://localhost:8000/static/js/grid/setup.js",
                                                  "http://localhost:8000/static/js/grid/mouse_events.js",
                                                  "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js",
                                                  "http://code.jquery.com/ui/1.10.4/jquery-ui.js",
                                                  "http://localhost:8000/static/js/common/cookieHandler.js",
                                                  "http://localhost:8000/static/js/grid/generate_grid.js",
                                                  "http://localhost:8000/static/js/common/objects/course.js",
                                                  "http://localhost:8000/static/js/common/objects/section.js",
                                                  "http://localhost:8000/static/js/common/facebook_login.js",
                                                  "http://localhost:8000/static/js/common/utilities/util.js"])