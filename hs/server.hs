{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.ByteString.Lazy.Char8
import Data.ByteString.Char8
import Data.String
import Control.Monad    (msum)
import Happstack.Server
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import MakeElements

masterTemplate :: String -> [H.Html] -> H.Html -> H.Html
masterTemplate title headers body =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        sequence_ headers
        insertTimetableLinks
      H.body $ do
        body
        insertTimetableScripts


gridResponse :: ServerPart Response
gridResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Grid"
                [H.meta ! A.name "keywords"
                        ! A.content ""
                ]
                (do  insertGridHeader

                     insertConflictDialog

                     createTag H.div "" "row main" $ do
                         insertCoursePanel

                         insertSearchPanel

                         insertInfoPanel
                )

-- Create <links/>
insertTimetableLinks :: H.Html
insertTimetableLinks = do makeLink "stylesheet" "text/css" "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
                          makeLink "stylesheet" "text/css" "static/style/grid/timetable_styles.css"

-- Create <scripts/>
insertTimetableScripts :: H.Html
insertTimetableScripts = do insertjQuery
                            makeScript "static/js/grid/timetable_util.js"
                            makeScript "static/js/grid/setup.js"
                            makeScript "static/js/grid/mouse_events.js"
                            makeScript "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
                            makeScript "http://code.jquery.com/ui/1.10.4/jquery-ui.js"
                            makeScript "static/js/common/cookieHandler.js"
                            makeScript "static/js/grid/generate_grid.js"
                            makeScript "static/js/common/objects/course.js"
                            makeScript "static/js/common/objects/section.js"
                            makeScript "static/js/common/utilities/util.js"

-- Insert the header of the Grid. This contains the year of the timetable, and
-- a link back to the Graph.
insertGridHeader :: H.Html
insertGridHeader =  createTag H.div "" "row header" $ do
                            createTag H.div "header" "col-md-6 col-xs-6" $ do
                                    createTag H.h2 "" "" "2014-2015 Timetable"
                            createTag H.div "" "col-md-6 col-xs-6" $ do
                                    makeA "" "" "static/planner.html" $ do
                                          createTag H.h2 "home-link" "" "Back to Graph"

insertConflictDialog :: H.Html
insertConflictDialog = createTag H.div "dialog" "" "Conflicting courses are difficult to manage. Make sure you understand the added responsibility of having two or more conflicting courses."

insertCoursePanel :: H.Html
insertCoursePanel = createTag H.div "course-select-wrapper" "col-md-2 col-xs-6" $ do
                            createTag H.ul "course-select" "trapScroll-enabled" $ do
                                   createTag H.li "clear-all" "" $ do
                                          createTag H.h3 "" "" "Clear All"

insertSearchPanel :: H.Html
insertSearchPanel =  createTag H.div "search-layout" "col-md-2 col-xs-6 col-md-push-8" $ do
                             createTag H.div "filter-container" "" $ do
                                     makeForm "" "" "return false;" $ do
                                     makeInput "course-filter" "form-control" "Enter a course!" "off" "text"
                             createTag H.div "search-container" "" $ do
                                     createTag H.div "search-list" "" ""

insertInfoPanel :: H.Html
insertInfoPanel = createTag H.div "" "col-md-8 col-xs-12 col-md-pull-2" $ do
                            createTag H.div "info" "row" $ do
                                    createTag H.div "info-layout" "" $ do
                                            createTag H.div "" "" $ do
                                                   createTag H.span "course-info-code" "" ""
                                                   createTag H.span "course-info-title" "" ""

                                            createTag H.h4 "" "" $ do
                                                   createTag H.span "section-stats-section" "" ""
                                                   createTag H.span "section-stats-instructor" "" ""

                                            createTag H.p "section-stats-enrol" "" ""

insertjQuery :: H.Html
insertjQuery = makeScript "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"

graphResponse :: ServerPart Response
graphResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Svg serving test!"
                [H.meta ! A.name "keywords"
                        ! A.content ""
                ]
                (do  insertGridHeader

                     insertSVG "static/graph_regions.svg"

                     createTag H.div "" "row main" $ do
                           insertSVG "static/graph_regions.svg"
                )


graph :: String
graph = "planner.html"

grid :: String
grid = "timetable_creator.html"

static :: String
static = "static"

staticDir :: String
staticDir = "/home/cynic/courseography"

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir grid $ graphResponse,
         dir graph $ gridResponse,
         dir static $ serveDirectory EnableBrowsing [] staticDir
       ]